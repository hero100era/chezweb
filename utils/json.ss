;;; utils/json.ss - JSON parsing and encoding

(library (utils json)
  (export
    json-parse
    json-encode
    json-ref)

  (import (chezscheme)
          (utils helpers))

  ;; ----- JSON Parser -----

  (define (json-parse str)
    (let ([port (open-input-string str)])
      (guard (e [else (close-input-port port) (raise e)])
        (let ([result (parse-value port)])
          (close-input-port port)
          result))))

  (define (parse-value port)
    (skip-whitespace port)
    (let ([c (peek-char port)])
      (cond
        [(eof-object? c) (json-error "Unexpected end of input")]
        [(char=? c #\{) (parse-object port)]
        [(char=? c #\[) (parse-array port)]
        [(char=? c #\") (parse-json-string port)]
        [(char=? c #\t) (parse-true port)]
        [(char=? c #\f) (parse-false port)]
        [(char=? c #\n) (parse-null port)]
        [(or (char=? c #\-) (char-numeric? c)) (parse-number port)]
        [else (json-error (format "Unexpected character: ~a" c))])))

  (define (json-error msg)
    (error 'json-parse msg))

  (define (parse-object port)
    (read-char port)  ; consume '{'
    (skip-whitespace port)
    (if (char=? (peek-char port) #\})
        (begin (read-char port) '())
        (let loop ([result '()])
          (skip-whitespace port)
          (let ([key (parse-json-string port)])
            (skip-whitespace port)
            (expect-char port #\:)
            (let ([value (parse-value port)])
              (skip-whitespace port)
              (let ([c (read-char port)])
                (cond
                  [(char=? c #\})
                   (reverse (cons (cons (string->symbol key) value) result))]
                  [(char=? c #\,)
                   (loop (cons (cons (string->symbol key) value) result))]
                  [else
                   (json-error (format "Expected ',' or '}', got '~a'" c))])))))))

  (define (parse-array port)
    (read-char port)  ; consume '['
    (skip-whitespace port)
    (if (char=? (peek-char port) #\])
        (begin (read-char port) '#())
        (let loop ([result '()])
          (let ([value (parse-value port)])
            (skip-whitespace port)
            (let ([c (read-char port)])
              (cond
                [(char=? c #\]) (list->vector (reverse (cons value result)))]
                [(char=? c #\,) (loop (cons value result))]
                [else (json-error (format "Expected ',' or ']', got '~a'" c))]))))))

  (define (parse-json-string port)
    (read-char port)  ; consume '"'
    (let loop ([chars '()])
      (let ([c (read-char port)])
        (cond
          [(eof-object? c) (json-error "Unterminated string")]
          [(char=? c #\") (list->string (reverse chars))]
          [(char=? c #\\)
           (let ([escape (read-char port)])
             (cond
               [(eof-object? escape) (json-error "Unterminated escape")]
               [else
                (loop (cons (case escape
                              [(#\n) #\newline]
                              [(#\r) #\return]
                              [(#\t) #\tab]
                              [(#\\) #\\]
                              [(#\") #\"]
                              [(#\/) #\/]
                              [(#\b) #\backspace]
                              [(#\f) #\page]
                              [(#\u) (parse-unicode-escape port)]
                              [else escape])
                            chars))]))]
          [else (loop (cons c chars))]))))

  (define (parse-unicode-escape port)
    (let ([hex (list->string
                 (list (read-char port) (read-char port)
                       (read-char port) (read-char port)))])
      (let ([code (string->number hex 16)])
        (if code
            (integer->char code)
            (json-error (format "Invalid unicode escape: ~a" hex))))))

  (define (parse-number port)
    (let loop ([chars '()])
      (let ([c (peek-char port)])
        (if (or (eof-object? c)
                (not (or (char-numeric? c)
                        (memv c '(#\. #\- #\+ #\e #\E)))))
            (let* ([s (list->string (reverse chars))]
                   [n (string->number s)])
              (or n (json-error (format "Invalid number: ~a" s))))
            (begin
              (read-char port)
              (loop (cons c chars)))))))

  (define (parse-true port)
    (expect-literal port "true")
    #t)

  (define (parse-false port)
    (expect-literal port "false")
    #f)

  (define (parse-null port)
    (expect-literal port "null")
    'null)

  (define (skip-whitespace port)
    (let loop ()
      (let ([c (peek-char port)])
        (when (and (not (eof-object? c)) (char-whitespace? c))
          (read-char port)
          (loop)))))

  (define (expect-char port expected)
    (let ([c (read-char port)])
      (unless (and (char? c) (char=? c expected))
        (json-error (format "Expected '~a', got '~a'" expected c)))))

  (define (expect-literal port expected)
    (string-for-each
      (lambda (expected-char)
        (let ([c (read-char port)])
          (unless (and (char? c) (char=? c expected-char))
            (json-error (format "Expected '~a'" expected)))))
      expected))

  ;; ----- JSON Encoder -----

  (define (json-encode value)
    (call-with-string-output-port
      (lambda (port)
        (encode-value value port))))

  (define (encode-value value port)
    (cond
      [(eq? value 'null) (display "null" port)]
      [(eqv? value #t) (display "true" port)]
      [(eqv? value #f) (display "false" port)]
      [(and (number? value) (real? value) (or (nan? value) (infinite? value)))
       (display "null" port)]
      [(number? value) (display value port)]
      [(string? value) (encode-json-string value port)]
      [(symbol? value) (encode-json-string (symbol->string value) port)]
      [(vector? value) (encode-array value port)]
      [(list? value) (encode-object value port)]
      [else (error 'json-encode (format "Cannot encode: ~a" value))]))

  (define (encode-json-string str port)
    (display "\"" port)
    (string-for-each
      (lambda (c)
        (cond
          [(char=? c #\") (display "\\\"" port)]
          [(char=? c #\\) (display "\\\\" port)]
          [(char=? c #\newline) (display "\\n" port)]
          [(char=? c #\return) (display "\\r" port)]
          [(char=? c #\tab) (display "\\t" port)]
          [(char=? c #\backspace) (display "\\b" port)]
          [(char=? c #\page) (display "\\f" port)]
          [(char<? c #\space)
           (let ([code (char->integer c)])
             (fprintf port "\\u~a" (format-number code 4)))]
          [else (display c port)]))
      str)
    (display "\"" port))

  (define (encode-array vec port)
    (display "[" port)
    (let ([len (vector-length vec)])
      (do ([i 0 (+ i 1)])
          ((>= i len))
        (when (> i 0) (display "," port))
        (encode-value (vector-ref vec i) port)))
    (display "]" port))

  (define (encode-object alist port)
    (display "{" port)
    (let loop ([pairs alist] [first? #t])
      (unless (null? pairs)
        (let ([pair (car pairs)])
          (unless first? (display "," port))
          (encode-json-string (if (symbol? (car pair))
                                  (symbol->string (car pair))
                                  (car pair))
                              port)
          (display ":" port)
          (encode-value (cdr pair) port))
        (loop (cdr pairs) #f)))
    (display "}" port))

  ;; ----- JSON Accessor -----

  (define (json-ref obj . keys)
    (let loop ([obj obj] [keys keys])
      (if (null? keys)
          obj
          (let ([key (car keys)])
            (cond
              [(and (list? obj) (or (symbol? key) (string? key)))
               (let ([pair (assq (if (string? key) (string->symbol key) key) obj)])
                 (if pair
                     (loop (cdr pair) (cdr keys))
                     #f))]
              [(and (vector? obj) (integer? key))
               (if (and (>= key 0) (< key (vector-length obj)))
                   (loop (vector-ref obj key) (cdr keys))
                   #f)]
              [else #f])))))
)
