;;; core/server.ss - HTTP server using libuv

(library (core server)
  (export
    make-http-server
    http-server?
    server-listen
    server-close)

  (import (chezscheme)
          (ffi libuv)
          (utils helpers)
          (core request)
          (core response))

  ;; Server record with protocol
  (define-record-type http-server
    (fields
      loop
      handle
      handler
      (mutable running?)
      (mutable clients))
    (protocol
      (lambda (new)
        (lambda (handler)
          (let* ([loop (uv-default-loop)]
                 [handle (alloc-uv-tcp loop)])
            (if handle
                (new loop handle handler #f (make-eq-hashtable))
                (error 'make-http-server "Failed to initialize TCP handle")))))))

  ;; Client connection state
  (define-record-type http-client
    (fields
      handle
      (mutable buffer)))

  ;; Constants
  (define DEFAULT-BACKLOG 128)
  (define BUFFER-SIZE 65536)

  ;; Allocation callback for libuv
  (define (create-alloc-callback)
    (make-alloc-cb
      (lambda (handle suggested-size buf)
        (let ([base (foreign-alloc suggested-size)])
          (uv-buf-base-set! buf base)
          (uv-buf-len-set! buf suggested-size)))))

  ;; Read callback factory
  (define (create-read-callback server)
    (make-read-cb
      (lambda (stream nread buf)
        (let ([base (uv-buf-base buf)])
          (cond
            ;; Error or EOF
            [(< nread 0)
             (when (> base 0)
               (foreign-free base))
             (close-client server stream)]

            ;; Data received
            [(> nread 0)
             (let* ([data-bv (make-bytevector-from-foreign base nread)]
                    [data-str (bytevector->latin-1-string data-bv)]
                    [client (hashtable-ref (http-server-clients server) stream #f)])
               (foreign-free base)
               (when client
                 (let ([new-buffer (string-append (http-client-buffer client) data-str)])
                   (http-client-buffer-set! client new-buffer)
                   (when (request-complete? new-buffer)
                     (handle-request server stream new-buffer)
                     (http-client-buffer-set! client "")))))]

            ;; nread == 0, do nothing
            [else
             (when (> base 0)
               (foreign-free base))])))))

  ;; Handle incoming request
  (define (handle-request server stream data)
    (let ([request (parse-http-request data)])
      (if request
          (let* ([handler (http-server-handler server)]
                 [response (guard (e [else
                                      (print-exception e)
                                      (make-error-response 500 "Internal Server Error")])
                             (handler request))]
                 [response-bv (response->bytevector response)])
            (send-response stream response-bv server))
          ;; Bad request
          (let ([response-bv (response->bytevector
                              (make-error-response 400 "Bad Request"))])
            (send-response stream response-bv server)))))

  (define (print-exception e)
    (fprintf (current-error-port) "Error: ")
    (display-condition e (current-error-port))
    (newline (current-error-port)))

  ;; Send response and close connection
  (define (send-response stream response-bv server)
    (let* ([len (bytevector-length response-bv)]
           [base (foreign-alloc len)]
           [req (alloc-uv-write-req)]
           [buf (make-uv-buf)])
      ;; Copy response to foreign memory
      (copy-bytevector-to-foreign response-bv base)
      ;; Set up buffer
      (uv-buf-base-set! buf base)
      (uv-buf-len-set! buf len)
      ;; Write
      (uv-write req stream buf 1
                (make-write-cb
                  (lambda (req status)
                    ;; Clean up write request and buffer
                    (let ([buf-base (uv-buf-base buf)])
                      (when (> buf-base 0)
                        (foreign-free buf-base)))
                    (foreign-free buf)
                    (foreign-free req)
                    ;; Close connection (HTTP/1.0 style for simplicity)
                    (close-client server stream))))))

  ;; Close client connection
  (define (close-client server stream)
    (uv-close stream
              (make-close-cb
                (lambda (handle)
                  (hashtable-delete! (http-server-clients server) handle)
                  (foreign-free handle)))))

  ;; Connection callback factory
  (define (create-connection-callback server)
    (make-connection-cb
      (lambda (server-handle status)
        (when (= status 0)
          (let ([client-handle (alloc-uv-tcp (http-server-loop server))])
            (when client-handle
              (when (= (uv-accept server-handle client-handle) 0)
                ;; Track client
                (hashtable-set! (http-server-clients server)
                               client-handle
                               (make-http-client client-handle ""))
                ;; Start reading
                (uv-read-start client-handle
                              (create-alloc-callback)
                              (create-read-callback server)))))))))

  ;; Start listening
  (define (server-listen server host port)
    (let* ([loop (http-server-loop server)]
           [handle (http-server-handle server)]
           [addr (foreign-alloc 128)])  ; Enough for sockaddr_in6

      ;; Set up address
      (let ([result (uv-ip4-addr host port addr)])
        (when (< result 0)
          (foreign-free addr)
          (error 'server-listen "Failed to parse address" host port)))

      ;; Bind
      (let ([result (uv-tcp-bind handle addr 0)])
        (foreign-free addr)
        (when (< result 0)
          (error 'server-listen "Failed to bind" result)))

      ;; Listen
      (let ([result (uv-listen handle DEFAULT-BACKLOG
                              (create-connection-callback server))])
        (when (< result 0)
          (error 'server-listen "Failed to listen" result)))

      (http-server-running?-set! server #t)
      (printf " * Serving on http://~a:~a/\n" host port)
      (printf " * Press Ctrl+C to quit\n")

      ;; Run event loop
      (uv-run loop UV_RUN_DEFAULT)))

  ;; Close server
  (define (server-close server)
    (when (http-server-running? server)
      (http-server-running?-set! server #f)
      ;; Close all clients
      (let ([clients (http-server-clients server)])
        (vector-for-each
          (lambda (handle)
            (close-client server handle))
          (hashtable-keys clients)))
      ;; Close server handle
      (uv-close (http-server-handle server)
                (make-close-cb
                  (lambda (handle)
                    (foreign-free handle))))
      ;; Stop loop
      (uv-stop (http-server-loop server))))
)
