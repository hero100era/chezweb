;;; core/response.ss - HTTP Response building

(library (core response)
  (export
    make-response
    response?
    response-status
    response-headers
    response-body
    response-status-set!
    response-headers-set!
    response-body-set!

    response-set-status!
    response-set-header!
    response-set-body!
    response-set-cookie!
    response-get-header
    response->string
    response->bytevector

    ;; Convenience constructors
    make-html-response
    make-json-response
    make-text-response
    make-redirect-response
    make-error-response

    ;; Status utilities
    status-message)

  (import (chezscheme)
          (utils helpers)
          (utils json))

  ;; Response record - use protocol to customize constructor
  (define-record-type response
    (fields
      (mutable status)
      (mutable headers)
      (mutable body))
    (protocol
      (lambda (new)
        (case-lambda
          [() (new 200 '() "")]
          [(status) (new status '() "")]
          [(status headers) (new status headers "")]
          [(status headers body) (new status headers body)]))))

  ;; Set status
  (define (response-set-status! resp status)
    (response-status-set! resp status))

  ;; Get header
  (define (response-get-header resp name)
    (assoc-ref (response-headers resp) (string-downcase name)))

  ;; Set header (replaces existing)
  (define (response-set-header! resp name value)
    (let* ([headers (response-headers resp)]
           [name-lower (string-downcase name)]
           [new-headers (cons (cons name-lower value)
                             (remp (lambda (h)
                                     (and (pair? h)
                                          (string? (car h))
                                          (string=? (car h) name-lower)))
                                   headers))])
      (response-headers-set! resp new-headers)))

  ;; Set body and update content-length
  (define (response-set-body! resp body)
    (response-body-set! resp body)
    (response-set-header! resp "content-length"
                          (number->string
                            (if (string? body)
                                (bytevector-length (string->utf8 body))
                                (bytevector-length body)))))

  ;; Set cookie
  (define (response-set-cookie! resp name value . options)
    (let ([cookie-str (build-cookie-string name value options)])
      (response-set-header! resp "set-cookie" cookie-str)))

  ;; Build cookie string from options
  (define (build-cookie-string name value options)
    (let loop ([opts options] [parts (list (string-append name "=" value))])
      (if (null? opts)
          (string-join (reverse parts) "; ")
          (let ([opt (car opts)])
            (cond
              [(and (not (null? (cdr opts))) (eq? opt 'max-age:))
               (loop (cddr opts)
                     (cons (format "Max-Age=~a" (cadr opts)) parts))]
              [(and (not (null? (cdr opts))) (eq? opt 'path:))
               (loop (cddr opts)
                     (cons (format "Path=~a" (cadr opts)) parts))]
              [(and (not (null? (cdr opts))) (eq? opt 'domain:))
               (loop (cddr opts)
                     (cons (format "Domain=~a" (cadr opts)) parts))]
              [(eq? opt 'secure)
               (loop (cdr opts) (cons "Secure" parts))]
              [(eq? opt 'httponly)
               (loop (cdr opts) (cons "HttpOnly" parts))]
              [(eq? opt 'samesite-strict)
               (loop (cdr opts) (cons "SameSite=Strict" parts))]
              [(eq? opt 'samesite-lax)
               (loop (cdr opts) (cons "SameSite=Lax" parts))]
              [else (loop (cdr opts) parts)])))))

  ;; Status code to message mapping
  (define status-messages
    '((100 . "Continue")
      (101 . "Switching Protocols")
      (200 . "OK")
      (201 . "Created")
      (202 . "Accepted")
      (204 . "No Content")
      (301 . "Moved Permanently")
      (302 . "Found")
      (303 . "See Other")
      (304 . "Not Modified")
      (307 . "Temporary Redirect")
      (308 . "Permanent Redirect")
      (400 . "Bad Request")
      (401 . "Unauthorized")
      (403 . "Forbidden")
      (404 . "Not Found")
      (405 . "Method Not Allowed")
      (406 . "Not Acceptable")
      (408 . "Request Timeout")
      (409 . "Conflict")
      (410 . "Gone")
      (413 . "Payload Too Large")
      (414 . "URI Too Long")
      (415 . "Unsupported Media Type")
      (422 . "Unprocessable Entity")
      (429 . "Too Many Requests")
      (500 . "Internal Server Error")
      (501 . "Not Implemented")
      (502 . "Bad Gateway")
      (503 . "Service Unavailable")
      (504 . "Gateway Timeout")))

  (define (status-message code)
    (let ([pair (assv code status-messages)])
      (if pair (cdr pair) "Unknown")))

  ;; Convert response to HTTP string
  (define (response->string resp)
    (let* ([status (response-status resp)]
           [status-line (format "HTTP/1.1 ~a ~a\r\n" status (status-message status))]
           [headers (response-headers resp)]
           [header-lines (if (null? headers)
                            ""
                            (string-append
                              (string-join
                                (map (lambda (h)
                                       (format "~a: ~a" (car h) (cdr h)))
                                     headers)
                                "\r\n")
                              "\r\n"))]
           [body (response-body resp)])
      (string-append
        status-line
        header-lines
        "\r\n"
        (if (string? body) body (bytevector->latin-1-string body)))))

  ;; Convert response to bytevector
  (define (response->bytevector resp)
    (latin-1-string->bytevector (response->string resp)))

  ;; ----- Convenience Constructors -----

  (define (make-html-response body . status)
    (let ([resp (make-response)])
      (response-set-status! resp (if (null? status) 200 (car status)))
      (response-set-header! resp "content-type" "text/html; charset=utf-8")
      (response-set-body! resp body)
      resp))

  (define (make-json-response data . status)
    (let ([resp (make-response)]
          [body (json-encode data)])
      (response-set-status! resp (if (null? status) 200 (car status)))
      (response-set-header! resp "content-type" "application/json; charset=utf-8")
      (response-set-body! resp body)
      resp))

  (define (make-text-response body . status)
    (let ([resp (make-response)])
      (response-set-status! resp (if (null? status) 200 (car status)))
      (response-set-header! resp "content-type" "text/plain; charset=utf-8")
      (response-set-body! resp body)
      resp))

  (define (make-redirect-response url . status)
    (let ([resp (make-response)])
      (response-set-status! resp (if (null? status) 302 (car status)))
      (response-set-header! resp "location" url)
      (response-set-body! resp "")
      resp))

  (define (make-error-response status message)
    (let ([resp (make-response)]
          [body (format
"<!DOCTYPE html>
<html>
<head><title>~a ~a</title></head>
<body>
<h1>~a ~a</h1>
<p>~a</p>
</body>
</html>"
                 status (status-message status)
                 status (status-message status)
                 message)])
      (response-set-status! resp status)
      (response-set-header! resp "content-type" "text/html; charset=utf-8")
      (response-set-body! resp body)
      resp))
)
