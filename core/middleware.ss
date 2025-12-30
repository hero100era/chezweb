;;; core/middleware.ss - Middleware system

(library (core middleware)
  (export
    make-middleware-stack
    middleware-stack?
    middleware-use!
    middleware-run

    ;; Built-in middleware
    make-logger-middleware
    make-cors-middleware
    make-static-middleware)

  (import (chezscheme)
          (utils helpers)
          (core request)
          (core response))

  ;; Middleware stack with protocol
  (define-record-type middleware-stack
    (fields
      (mutable middlewares))
    (protocol
      (lambda (new)
        (lambda ()
          (new '())))))

  ;; Add middleware to stack
  (define (middleware-use! stack mw)
    (middleware-stack-middlewares-set!
      stack
      (append (middleware-stack-middlewares stack) (list mw))))

  ;; Run middleware stack with final handler
  (define (middleware-run stack request final-handler)
    (let ([mws (middleware-stack-middlewares stack)])
      (if (null? mws)
          (final-handler request)
          (let build-chain ([mws (reverse mws)] [next final-handler])
            (if (null? mws)
                (next request)
                (let ([mw (car mws)])
                  (build-chain (cdr mws)
                              (lambda (req) (mw req next)))))))))

  ;; ----- Logger Middleware -----

  (define (make-logger-middleware . args)
    (let ([out-port (if (null? args) (current-output-port) (car args))])
      (lambda (request next)
        (let* ([method (request-method request)]
               [path (request-path request)]
               [start (current-time 'time-monotonic)])
          (fprintf out-port "[~a] ~a ~a\n"
                   (format-log-timestamp)
                   method
                   path)
          (flush-output-port out-port)
          (let ([resp (next request)])
            (let* ([end (current-time 'time-monotonic)]
                   [elapsed-ns (- (+ (* (time-second end) 1000000000)
                                    (time-nanosecond end))
                                 (+ (* (time-second start) 1000000000)
                                    (time-nanosecond start)))]
                   [elapsed-ms (/ elapsed-ns 1000000.0)])
              (fprintf out-port "  -> ~a (~,2fms)\n"
                       (response-status resp)
                       elapsed-ms)
              (flush-output-port out-port))
            resp)))))

  (define (format-log-timestamp)
    (let ([d (current-date)])
      (format "~a-~a-~a ~a:~a:~a"
              (date-year d)
              (pad-zero (date-month d))
              (pad-zero (date-day d))
              (pad-zero (date-hour d))
              (pad-zero (date-minute d))
              (pad-zero (date-second d)))))

  ;; ----- CORS Middleware -----

  ;; Options is an alist: ((origin . "*") (methods . "GET, POST"))
  (define (make-cors-middleware . args)
    (let ([options (if (and (pair? args) (list? (car args)))
                       (car args)
                       '())])
      (let ([allow-origin (or (assoc-ref options 'origin) "*")]
            [allow-methods (or (assoc-ref options 'methods) "GET, POST, PUT, DELETE, PATCH, OPTIONS")]
            [allow-headers (or (assoc-ref options 'headers) "Content-Type, Authorization, X-Requested-With")]
            [max-age (or (assoc-ref options 'max-age) "86400")]
            [credentials? (assoc-ref options 'credentials)])
        (lambda (request next)
          (if (eq? (request-method request) 'OPTIONS)
              (let ([resp (make-response)])
                (response-set-status! resp 204)
                (set-cors-headers! resp allow-origin allow-methods
                                  allow-headers max-age credentials?)
                resp)
              (let ([resp (next request)])
                (set-cors-headers! resp allow-origin allow-methods
                                  allow-headers max-age credentials?)
                resp))))))

  (define (set-cors-headers! resp origin methods headers max-age credentials?)
    (response-set-header! resp "access-control-allow-origin" origin)
    (response-set-header! resp "access-control-allow-methods" methods)
    (response-set-header! resp "access-control-allow-headers" headers)
    (response-set-header! resp "access-control-max-age" max-age)
    (when credentials?
      (response-set-header! resp "access-control-allow-credentials" "true")))

  ;; ----- Static Files Middleware -----

  (define (make-static-middleware url-prefix directory)
    (lambda (request next)
      (let ([path (request-path request)])
        (if (string-prefix? url-prefix path)
            (let* ([rel-path (substring path
                                       (string-length url-prefix)
                                       (string-length path))]
                   [file-path (string-append directory
                                            (if (or (string=? rel-path "")
                                                    (string-prefix? "/" rel-path))
                                                rel-path
                                                (string-append "/" rel-path)))]
                   [content (safe-read-file file-path)])
              (if content
                  (let ([resp (make-response)]
                        [mime (get-mime-type file-path)])
                    (response-set-header! resp "content-type" mime)
                    (response-set-body! resp content)
                    resp)
                  (next request)))
            (next request)))))

  (define (safe-read-file path)
    (guard (e [else #f])
      (call-with-input-file path get-string-all)))

  (define (get-mime-type path)
    (let ([ext (get-file-extension path)])
      (cond
        [(member ext '("html" "htm")) "text/html; charset=utf-8"]
        [(string=? ext "css") "text/css; charset=utf-8"]
        [(string=? ext "js") "application/javascript; charset=utf-8"]
        [(string=? ext "json") "application/json; charset=utf-8"]
        [(string=? ext "xml") "application/xml; charset=utf-8"]
        [(string=? ext "txt") "text/plain; charset=utf-8"]
        [(string=? ext "png") "image/png"]
        [(member ext '("jpg" "jpeg")) "image/jpeg"]
        [(string=? ext "gif") "image/gif"]
        [(string=? ext "svg") "image/svg+xml"]
        [(string=? ext "ico") "image/x-icon"]
        [(string=? ext "webp") "image/webp"]
        [(string=? ext "woff") "font/woff"]
        [(string=? ext "woff2") "font/woff2"]
        [(string=? ext "ttf") "font/ttf"]
        [(string=? ext "pdf") "application/pdf"]
        [(string=? ext "zip") "application/zip"]
        [else "application/octet-stream"])))

  (define (get-file-extension path)
    (let ([dot-pos (string-rindex path #\.)])
      (if dot-pos
          (string-downcase (substring path (+ dot-pos 1) (string-length path)))
          "")))
)
