;;; core/request.ss - HTTP Request parsing

(library (core request)
  (export
    make-request
    request?
    request-method
    request-path
    request-query-string
    request-headers
    request-body
    request-params
    request-params-set!

    request-query-params
    request-form-data
    request-form
    request-files
    request-json-body
    request-cookies
    request-get-header

    parse-http-request
    request-complete?)

  (import (chezscheme)
          (utils helpers)
          (utils url)
          (utils multipart)
          (utils json))

  ;; Request record with protocol for custom constructor
  (define-record-type request
    (fields
      method
      path
      query-string
      headers
      body
      (mutable params)           ; URL params from routing
      (mutable cached-query)     ; Lazy parsed query params
      (mutable cached-form)      ; Lazy parsed form data
      (mutable cached-files)     ; Lazy parsed files
      (mutable cached-json)      ; Lazy parsed JSON body
      (mutable cached-cookies))  ; Lazy parsed cookies
    (protocol
      (lambda (new)
        (lambda (method path query-string headers body)
          (new method path query-string headers body
               '() #f #f #f #f #f)))))

  ;; Parse HTTP request from string
  (define (parse-http-request data)
    (let* ([header-end (string-contains data "\r\n\r\n")]
           [header-section (if header-end
                              (substring data 0 header-end)
                              data)]
           [body (if header-end
                    (substring data (+ header-end 4) (string-length data))
                    "")]
           [lines (string-split header-section "\r\n")])

      (if (null? lines)
          #f
          (let* ([request-line (car lines)]
                 [parts (string-split request-line " ")])
            (if (< (length parts) 2)
                #f
                (let* ([method (string->symbol (string-upcase (car parts)))]
                       [full-path (cadr parts)]
                       [path-query (string-split full-path "?")]
                       [path (car path-query)]
                       [query-string (if (> (length path-query) 1)
                                        (string-join (cdr path-query) "?")
                                        "")]
                       [headers (parse-headers (cdr lines))])
                  (make-request method path query-string headers body)))))))

  ;; Parse headers from lines
  (define (parse-headers lines)
    (let loop ([lines lines] [headers '()])
      (if (or (null? lines) (string=? (car lines) ""))
          (reverse headers)
          (let* ([line (car lines)]
                 [colon-pos (string-index line #\:)])
            (if colon-pos
                (let ([key (string-downcase
                            (string-trim (substring line 0 colon-pos)))]
                      [value (string-trim
                              (substring line (+ colon-pos 1) (string-length line)))])
                  (loop (cdr lines) (cons (cons key value) headers)))
                (loop (cdr lines) headers))))))

  ;; Get header by name (case-insensitive)
  (define (request-get-header req name)
    (assoc-ref (request-headers req) (string-downcase name)))

  ;; Get/parse query parameters (lazy)
  (define (request-query-params req)
    (or (request-cached-query req)
        (let ([params (parse-query-string (request-query-string req))])
          (request-cached-query-set! req params)
          params)))

  ;; Get/parse form data (lazy)
  (define (request-form-data req)
    (ensure-parsed-body! req)
    (request-cached-form req))

  ;; Get/parse files (lazy)
  (define (request-files req)
    (ensure-parsed-body! req)
    (request-cached-files req))

  ;; Convenience wrapper for form data
  (define (request-form req)
    (request-form-data req))

  ;; Helper to ensure body is parsed (lazy)
  (define (ensure-parsed-body! req)
    (unless (request-cached-form req)
      (let* ([content-type (request-get-header req "content-type")]
             [form '()]
             [files '()])
        (cond
          [(and content-type (string-contains content-type "application/x-www-form-urlencoded"))
           (set! form (parse-query-string (request-body req)))]
          [(and content-type (string-contains content-type "multipart/form-data"))
           (let ([boundary (extract-boundary content-type)])
             (if boundary
                 (let ([parts (parse-multipart-data (request-body req) boundary)])
                   (set! form (filter (lambda (p) (string? (cdr p))) parts))
                   (set! files (filter (lambda (p) (list? (cdr p))) parts)))
                 '()))])
        (request-cached-form-set! req form)
        (request-cached-files-set! req files))))

  ;; Extract boundary from Content-Type header
  (define (extract-boundary content-type)
    (let ([start (string-contains content-type "boundary=")])
      (if start
          (substring content-type (+ start 9) (string-length content-type))
          #f)))

  ;; Get/parse JSON body (lazy)
  (define (request-json-body req)
    (or (request-cached-json req)
        (let* ([content-type (request-get-header req "content-type")]
               [json-data (if (and content-type
                                  (string-contains content-type "application/json"))
                             (guard (e [else #f])
                               (json-parse (request-body req)))
                             #f)])
          (request-cached-json-set! req json-data)
          json-data)))

  ;; Get/parse cookies (lazy)
  (define (request-cookies req)
    (or (request-cached-cookies req)
        (let* ([cookie-header (request-get-header req "cookie")]
               [cookies (if cookie-header
                           (map (lambda (pair)
                                  (let ([parts (string-split (string-trim pair) "=")])
                                    (cons (string-trim (car parts))
                                          (if (> (length parts) 1)
                                              (string-trim (string-join (cdr parts) "="))
                                              ""))))
                                (string-split cookie-header ";"))
                           '())])
          (request-cached-cookies-set! req cookies)
          cookies)))
  ;; Check if request is complete (including body based on Content-Length)
  (define (request-complete? data)
    (let ([header-end (string-contains data "\r\n\r\n")])
      (if (not header-end)
          #f
          (let* ([headers-str (substring data 0 header-end)]
                 [lines (string-split headers-str "\r\n")])
            (if (null? lines)
                #f
                (let* ([headers (parse-headers (cdr lines))]
                       [content-length-str (assoc-ref headers "content-length")])
                  (if (not content-length-str)
                      #t ; No content-length, assume complete if headers are done
                      (let ([content-length (string->number content-length-str)])
                        (if (not content-length)
                            #t
                            (let ([body-len (- (string-length data) (+ header-end 4))])
                              (>= body-len content-length)))))))))))
)
