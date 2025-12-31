;;; ffi/libuv.ss - libuv bindings for Chez Scheme

(library (ffi libuv)
  (export
    ;; Loop
    uv-default-loop
    uv-run
    uv-stop

    ;; TCP
    uv-tcp-init
    uv-tcp-bind
    uv-listen
    uv-accept
    uv-read-start
    uv-write
    uv-close

    ;; Address
    uv-ip4-addr

    ;; Handle sizes
    uv-handle-size
    uv-req-size

    ;; Constants
    UV_RUN_DEFAULT
    UV_TCP
    UV_WRITE

    ;; Memory/Buffer helpers
    alloc-uv-tcp
    alloc-uv-write-req
    make-uv-buf
    uv-buf-base
    uv-buf-len
    uv-buf-base-set!
    uv-buf-len-set!

    ;; Callback creation
    make-connection-cb
    make-alloc-cb
    make-read-cb
    make-write-cb
    make-close-cb

    ;; Foreign memory helpers
    make-bytevector-from-foreign
    copy-bytevector-to-foreign)

  (import (chezscheme))

  ;; Load libuv - try different names
  (define libuv-lib
    (or (guard (e [else #f]) (load-shared-object "libuv.so.1"))
        (guard (e [else #f]) (load-shared-object "libuv.so"))
        (guard (e [else #f]) (load-shared-object "libuv.1.dylib"))
        (guard (e [else #f]) (load-shared-object "libuv.dylib"))
        (guard (e [else #f]) (load-shared-object "libuv.dll"))
        (error 'libuv "Could not load libuv shared library")))

  ;; Constants
  (define UV_RUN_DEFAULT 0)
  (define UV_RUN_ONCE 1)
  (define UV_RUN_NOWAIT 2)

  ;; Handle types
  (define UV_TCP 12)

  ;; Request types
  (define UV_WRITE 3)

  ;; ----- Loop Functions -----

  (define uv-default-loop
    (foreign-procedure "uv_default_loop" () void*))

  (define uv-run
    (foreign-procedure "uv_run" (void* int) int))

  (define uv-stop
    (foreign-procedure "uv_stop" (void*) void))

  ;; ----- TCP Functions -----

  (define uv-tcp-init
    (foreign-procedure "uv_tcp_init" (void* void*) int))

  (define uv-tcp-bind
    (foreign-procedure "uv_tcp_bind" (void* void* unsigned-int) int))

  (define uv-listen
    (foreign-procedure "uv_listen" (void* int void*) int))

  (define uv-accept
    (foreign-procedure "uv_accept" (void* void*) int))

  (define uv-read-start
    (foreign-procedure "uv_read_start" (void* void* void*) int))

  (define uv-write
    (foreign-procedure "uv_write" (void* void* void* unsigned-int void*) int))

  (define uv-close
    (foreign-procedure "uv_close" (void* void*) void))

  ;; ----- Address Functions -----

  (define uv-ip4-addr
    (foreign-procedure "uv_ip4_addr" (string int void*) int))

  ;; ----- Size Functions -----

  (define uv-handle-size
    (foreign-procedure "uv_handle_size" (int) size_t))

  (define uv-req-size
    (foreign-procedure "uv_req_size" (int) size_t))

  ;; ----- Memory Helpers -----

  (define (alloc-uv-tcp loop)
    (let* ([size (uv-handle-size UV_TCP)]
           [handle (foreign-alloc size)])
      (let ([result (uv-tcp-init loop handle)])
        (if (< result 0)
            (begin (foreign-free handle) #f)
            handle))))

  (define (alloc-uv-write-req)
    (foreign-alloc (uv-req-size UV_WRITE)))

  ;; uv_buf_t structure (platform dependent, typically 16 bytes on 64-bit)
  (define uv-buf-size 16)

  (define (make-uv-buf)
    (foreign-alloc uv-buf-size))

  (define (uv-buf-base buf)
    (foreign-ref 'void* buf 0))

  (define (uv-buf-len buf)
    (foreign-ref 'size_t buf 8))

  (define (uv-buf-base-set! buf val)
    (foreign-set! 'void* buf 0 val))

  (define (uv-buf-len-set! buf val)
    (foreign-set! 'size_t buf 8 val))

  ;; ----- Callback Helpers -----

  (define (make-connection-cb proc)
    (let ([code (foreign-callable proc (void* int) void)])
      (lock-object code)
      (foreign-callable-entry-point code)))

  (define (make-alloc-cb proc)
    (let ([code (foreign-callable proc (void* size_t void*) void)])
      (lock-object code)
      (foreign-callable-entry-point code)))

  (define (make-read-cb proc)
    (let ([code (foreign-callable proc (void* ssize_t void*) void)])
      (lock-object code)
      (foreign-callable-entry-point code)))

  (define (make-write-cb proc)
    (let ([code (foreign-callable proc (void* int) void)])
      (lock-object code)
      (foreign-callable-entry-point code)))

  (define (make-close-cb proc)
    (let ([code (foreign-callable proc (void*) void)])
      (lock-object code)
      (foreign-callable-entry-point code)))

  ;; ----- Bytevector/Foreign Memory -----

  (define (make-bytevector-from-foreign ptr len)
    (let ([bv (make-bytevector len)])
      (do ([i 0 (+ i 1)])
          ((>= i len) bv)
        (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i)))))

  (define (copy-bytevector-to-foreign bv ptr)
    (let ([len (bytevector-length bv)])
      (do ([i 0 (+ i 1)])
          ((>= i len) len)
        (foreign-set! 'unsigned-8 ptr i (bytevector-u8-ref bv i)))))
)
