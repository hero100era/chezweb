;;; chezweb.ss - Flask-like web framework for Chez Scheme

(library (chezweb)
  (export
    ;; Application
    make-app
    app?
    app-route
    app-get
    app-post
    app-put
    app-delete
    app-patch
    app-use
    app-run
    app-set-config!
    app-get-config

    ;; Request accessors
    request?
    request-method
    request-path
    request-query-string
    request-headers
    request-body
    request-params
    request-query-params
    request-form-data
    request-form
    request-files
    request-json-body
    request-cookies
    request-get-header

    ;; Response helpers
    html
    json-response
    text
    redirect
    error-page
    file-response

    ;; Response manipulation
    make-response
    response?
    response-status
    response-set-status!
    response-set-header!
    response-set-cookie!

    ;; Template
    render
    render-file

    ;; JSON utilities
    json-parse
    json-encode
    json-ref

    ;; URL utilities
    url-encode
    url-decode

    ;; Middleware
    make-logger-middleware
    make-cors-middleware
    make-static-middleware

    ;; Blueprint
    make-blueprint
    blueprint?
    blueprint-route
    blueprint-get
    blueprint-post
    blueprint-put
    blueprint-delete
    app-register-blueprint

    ;; Re-export helpers
    assoc-ref)

  (import (chezscheme)
          (utils helpers)
          (utils json)
          (utils url)
          (utils template)
          (core request)
          (core response)
          (core router)
          (core middleware)
          (core server))

  ;; ----- Application -----

  (define-record-type app
    (fields
      router
      middleware-stack
      (mutable config)
      (mutable error-handlers))
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-router)
               (make-middleware-stack)
               '()
               '())))))

  ;; Configuration
  (define (app-set-config! application key value)
    (app-config-set! application (alist-set (app-config application) key value)))

  (define (app-get-config application key . default)
    (let ([val (assoc-ref (app-config application) key)])
      (or val (if (null? default) #f (car default)))))

  ;; Routing
  (define (app-route application method pattern handler)
    (router-add-route! (app-router application) method pattern
                       (wrap-handler handler)))

  (define (app-get application pattern handler)
    (app-route application 'GET pattern handler))

  (define (app-post application pattern handler)
    (app-route application 'POST pattern handler))

  (define (app-put application pattern handler)
    (app-route application 'PUT pattern handler))

  (define (app-delete application pattern handler)
    (app-route application 'DELETE pattern handler))

  (define (app-patch application pattern handler)
    (app-route application 'PATCH pattern handler))

  ;; Middleware
  (define (app-use application mw)
    (middleware-use! (app-middleware-stack application) mw))

  ;; Wrap handler to normalize return values
  (define (wrap-handler handler)
    (lambda (request params)
      (request-params-set! request params)
      (let ([result (handler request)])
        (cond
          [(response? result) result]
          [(string? result) (html result)]
          [(or (list? result) (vector? result)) (json-response result)]
          [else (text (format "~a" result))]))))

  ;; Main request handler
  (define (create-app-handler application)
    (lambda (request)
      (let* ([method (request-method request)]
             [path (request-path request)]
             [match (router-match (app-router application) method path)])
        (if match
            (let ([handler (car match)]
                  [params (cdr match)])
              (middleware-run
                (app-middleware-stack application)
                request
                (lambda (req)
                  (guard (e [else (handle-app-error application e req)])
                    (handler req params)))))
            (let ([err-handler (assoc-ref (app-error-handlers application) 404)])
              (if err-handler
                  (err-handler request)
                  (error-page 404
                    (format "The requested URL ~a was not found." path))))))))

  ;; Error handling
  (define (handle-app-error application e request)
    (let ([msg (if (condition? e)
                   (call-with-string-output-port
                     (lambda (p) (display-condition e p)))
                   (format "~a" e))])
      (fprintf (current-error-port) "Error: ~a\n" msg)
      (let ([err-handler (assoc-ref (app-error-handlers application) 500)])
        (if err-handler
            (err-handler request)
            (error-page 500 msg)))))

  ;; Run the application
  ;; Options is an alist: ((host . "0.0.0.0") (port . 5000) (debug . #t))
  (define (app-run application . args)
    (let ([options (if (and (pair? args) (list? (car args)))
                       (car args)
                       '())])
      (let ([host (or (assoc-ref options 'host) "0.0.0.0")]
            [port (or (assoc-ref options 'port) 5000)]
            [debug (assoc-ref options 'debug)])

        (when debug
          (app-use application (make-logger-middleware)))

        (printf "\n")
        (printf " * Chez Web Application\n")
        (printf " * Debug mode: ~a\n" (if debug "on" "off"))

        (let ([server (make-http-server (create-app-handler application))])
          (server-listen server host port)))))

  ;; ----- Response Helpers -----

  (define (html body . status)
    (apply make-html-response body status))

  (define (json-response data . status)
    (apply make-json-response data status))

  (define (text body . status)
    (apply make-text-response body status))

  (define (redirect url . status)
    (apply make-redirect-response url status))

  (define (error-page status message)
    (make-error-response status message))

  (define (file-response path . args)
    (let ([options (if (and (pair? args) (list? (car args)))
                       (car args)
                       '())])
      (let ([content (guard (e [else #f])
                       (call-with-input-file path get-string-all))]
            [mime-type (or (assoc-ref options 'mime-type)
                          (guess-mime-type-for-file path))]
            [download-name (assoc-ref options 'download)])
        (if content
            (let ([resp (make-response)])
              (response-set-header! resp "content-type" mime-type)
              (when download-name
                (response-set-header! resp "content-disposition"
                  (format "attachment; filename=\"~a\"" download-name)))
              (response-set-body! resp content)
              resp)
            (error-page 404 "File not found")))))

  (define (guess-mime-type-for-file path)
    (let ([ext (let ([dot (string-rindex path #\.)])
                 (if dot
                     (string-downcase (substring path (+ dot 1) (string-length path)))
                     ""))])
      (cond
        [(member ext '("html" "htm")) "text/html; charset=utf-8"]
        [(string=? ext "css") "text/css"]
        [(string=? ext "js") "application/javascript"]
        [(string=? ext "json") "application/json"]
        [(string=? ext "png") "image/png"]
        [(member ext '("jpg" "jpeg")) "image/jpeg"]
        [(string=? ext "gif") "image/gif"]
        [(string=? ext "svg") "image/svg+xml"]
        [(string=? ext "pdf") "application/pdf"]
        [else "application/octet-stream"])))

  ;; Template helpers
  (define (render template-string context)
    (html (render-template template-string context)))

  (define (render-file template-path context)
    (html (render-template-file template-path context)))

  ;; ----- Blueprint -----

  (define-record-type blueprint
    (fields
      name
      url-prefix
      (mutable routes))
    (protocol
      (lambda (new)
        (case-lambda
          [(name) (new name "" '())]
          [(name prefix) (new name prefix '())]))))

  (define (blueprint-route bp method pattern handler)
    (blueprint-routes-set! bp
      (cons (list method pattern handler)
            (blueprint-routes bp))))

  (define (blueprint-get bp pattern handler)
    (blueprint-route bp 'GET pattern handler))

  (define (blueprint-post bp pattern handler)
    (blueprint-route bp 'POST pattern handler))

  (define (blueprint-put bp pattern handler)
    (blueprint-route bp 'PUT pattern handler))

  (define (blueprint-delete bp pattern handler)
    (blueprint-route bp 'DELETE pattern handler))

  (define (app-register-blueprint application bp)
    (let ([prefix (blueprint-url-prefix bp)])
      (for-each
        (lambda (route)
          (let ([method (car route)]
                [pattern (cadr route)]
                [handler (caddr route)])
            (app-route application method
                      (string-append prefix pattern)
                      handler)))
        (reverse (blueprint-routes bp)))))
)
