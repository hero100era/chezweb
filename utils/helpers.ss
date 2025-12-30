;;; utils/helpers.ss - Common helper functions (no dependencies)

(library (utils helpers)
  (export
    ;; String operations
    string-split
    string-contains
    string-index
    string-rindex
    string-trim
    string-trim-left
    string-trim-right
    string-prefix?
    string-suffix?
    string-join

    ;; List operations (use names that don't conflict with built-ins)
    assoc-ref
    alist-set
    alist-delete
    alist-update

    ;; Other utilities
    format-number
    pad-zero
    random-string)

  (import (chezscheme))

  ;; ----- String Operations -----

  ;; Split string by delimiter (string or char)
  (define (string-split str delim)
    (let* ([delim-str (if (char? delim) (string delim) delim)]
           [str-len (string-length str)]
           [delim-len (string-length delim-str)])
      (if (or (= str-len 0) (= delim-len 0))
          (list str)
          (let loop ([start 0] [i 0] [result '()])
            (cond
              [(> (+ i delim-len) str-len)
               (reverse (cons (substring str start str-len) result))]
              [(string=? (substring str i (+ i delim-len)) delim-str)
               (loop (+ i delim-len)
                     (+ i delim-len)
                     (cons (substring str start i) result))]
              [else
               (loop start (+ i 1) result)])))))

  ;; Find substring in string, return index or #f
  (define (string-contains str substr)
    (let ([str-len (string-length str)]
          [sub-len (string-length substr)])
      (cond
        [(= sub-len 0) 0]
        [(> sub-len str-len) #f]
        [else
         (let loop ([i 0])
           (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) i]
             [else (loop (+ i 1))]))])))

  ;; Find char in string from left, return index or #f
  (define (string-index str char)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) char) i]
          [else (loop (+ i 1))]))))

  ;; Find char in string from right, return index or #f
  (define (string-rindex str char)
    (let loop ([i (- (string-length str) 1)])
      (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (- i 1))])))

  ;; Trim whitespace from both ends
  (define (string-trim str)
    (string-trim-right (string-trim-left str)))

  ;; Trim whitespace from left
  (define (string-trim-left str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) ""]
          [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
          [else (substring str i len)]))))

  ;; Trim whitespace from right
  (define (string-trim-right str)
    (let loop ([i (- (string-length str) 1)])
      (cond
        [(< i 0) ""]
        [(char-whitespace? (string-ref str i)) (loop (- i 1))]
        [else (substring str 0 (+ i 1))])))

  ;; Check if string starts with prefix
  (define (string-prefix? prefix str)
    (let ([prefix-len (string-length prefix)]
          [str-len (string-length str)])
      (and (>= str-len prefix-len)
           (string=? (substring str 0 prefix-len) prefix))))

  ;; Check if string ends with suffix
  (define (string-suffix? suffix str)
    (let ([suffix-len (string-length suffix)]
          [str-len (string-length str)])
      (and (>= str-len suffix-len)
           (string=? (substring str (- str-len suffix-len) str-len) suffix))))

  ;; Join list of strings with delimiter
  (define (string-join strs delim)
    (cond
      [(null? strs) ""]
      [(null? (cdr strs)) (car strs)]
      [else
       (let loop ([strs (cdr strs)] [result (car strs)])
         (if (null? strs)
             result
             (loop (cdr strs) (string-append result delim (car strs)))))]))

  ;; ----- List/Alist Operations -----
  ;; Note: filter, map, etc. are built into Chez Scheme, so we don't redefine them

  ;; Get value from alist, with optional default
  (define (assoc-ref alist key . default)
    (let ([pair (cond
                  [(string? key)
                   (find (lambda (p)
                           (and (pair? p)
                                (string? (car p))
                                (string=? (car p) key)))
                         alist)]
                  [(symbol? key) (assq key alist)]
                  [else (assoc key alist)])])
      (if pair
          (cdr pair)
          (if (null? default) #f (car default)))))

  ;; Set value in alist (returns new alist)
  (define (alist-set alist key value)
    (cons (cons key value)
          (alist-delete alist key)))

  ;; Delete key from alist (returns new alist)
  (define (alist-delete alist key)
    (remp (lambda (p)
            (and (pair? p)
                 (cond
                   [(string? key) (and (string? (car p)) (string=? (car p) key))]
                   [(symbol? key) (eq? (car p) key)]
                   [else (equal? (car p) key)])))
          alist))

  ;; Update value in alist using a function
  (define (alist-update alist key proc default)
    (let ([current (assoc-ref alist key default)])
      (alist-set alist key (proc current))))

  ;; ----- Other Utilities -----

  ;; Format number with leading zeros
  (define (format-number n width)
    (let ([s (if (number? n) (number->string n) n)])
      (if (>= (string-length s) width)
          s
          (string-append (make-string (- width (string-length s)) #\0) s))))

  ;; Pad number to 2 digits
  (define (pad-zero n)
    (format-number n 2))

  ;; Generate random alphanumeric string
  (define (random-string len)
    (let ([chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"])
      (list->string
        (let loop ([i 0] [result '()])
          (if (>= i len)
              (reverse result)
              (loop (+ i 1)
                    (cons (string-ref chars (random (string-length chars)))
                          result)))))))
)
