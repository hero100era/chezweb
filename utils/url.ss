;;; utils/url.ss - URL parsing and encoding utilities

(library (utils url)
  (export
    url-decode
    url-encode
    parse-query-string
    build-query-string)

  (import (chezscheme)
          (utils helpers))

  ;; URL decode
  (define (url-decode str)
    (let ([len (string-length str)])
      (let loop ([i 0] [chars '()])
        (if (>= i len)
            (list->string (reverse chars))
            (let ([c (string-ref str i)])
              (cond
                [(char=? c #\%)
                 (if (<= (+ i 2) len)
                     (let ([code (string->number
                                   (substring str (+ i 1) (+ i 3))
                                   16)])
                       (if code
                           (loop (+ i 3) (cons (integer->char code) chars))
                           (loop (+ i 1) (cons c chars))))
                     (loop (+ i 1) (cons c chars)))]
                [(char=? c #\+)
                 (loop (+ i 1) (cons #\space chars))]
                [else
                 (loop (+ i 1) (cons c chars))]))))))

  ;; URL encode
  (define (url-encode str)
    (call-with-string-output-port
      (lambda (port)
        (string-for-each
          (lambda (c)
            (cond
              [(or (char-alphabetic? c)
                   (char-numeric? c)
                   (memv c '(#\- #\_ #\. #\~)))
               (display c port)]
              [(char=? c #\space)
               (display "+" port)]
              [else
               (let ([code (char->integer c)])
                 (display "%" port)
                 (when (< code 16) (display "0" port))
                 (display (string-upcase (number->string code 16)) port))]))
          str))))

  ;; Parse query string into alist
  (define (parse-query-string str)
    (if (or (not str) (string=? str ""))
        '()
        (map (lambda (pair)
               (let ([parts (string-split pair "=")])
                 (cons (url-decode (car parts))
                       (if (> (length parts) 1)
                           (url-decode (string-join (cdr parts) "="))
                           ""))))
             (string-split str "&"))))

  ;; Build query string from alist
  (define (build-query-string params)
    (if (null? params)
        ""
        (string-join
          (map (lambda (pair)
                 (string-append
                   (url-encode (if (symbol? (car pair))
                                   (symbol->string (car pair))
                                   (car pair)))
                   "="
                   (url-encode (cond
                                 [(string? (cdr pair)) (cdr pair)]
                                 [(number? (cdr pair)) (number->string (cdr pair))]
                                 [else (format "~a" (cdr pair))]))))
               params)
          "&")))
)
