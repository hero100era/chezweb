;;; utils/template.ss - Simple template engine

(library (utils template)
  (export
    render-template
    render-template-file)

  (import (chezscheme)
          (utils helpers))

  ;; Token type
  (define-record-type tmpl-token
    (fields type value))

  ;; Render template string with context
  (define (render-template template context)
    (let ([tokens (tokenize-template (open-input-string template))])
      (call-with-string-output-port
        (lambda (out)
          (render-tokens tokens context out)))))

  ;; Render template from file
  (define (render-template-file filename context)
    (let ([template (call-with-input-file filename get-string-all)])
      (render-template template context)))

  ;; Tokenize template into list of tokens
  (define (tokenize-template port)
    (let loop ([tokens '()])
      (let ([c (peek-char port)])
        (cond
          [(eof-object? c)
           (reverse tokens)]
          [(char=? c #\{)
           (read-char port)
           (let ([c2 (peek-char port)])
             (cond
               [(eof-object? c2)
                (reverse (cons (make-tmpl-token 'text "{") tokens))]
               [(char=? c2 #\{)
                (read-char port)
                (let ([expr (read-until-delim port "}}")])
                  (loop (cons (make-tmpl-token 'expr (string-trim expr)) tokens)))]
               [(char=? c2 #\%)
                (read-char port)
                (let ([tag (read-until-delim port "%}")])
                  (loop (cons (make-tmpl-token 'tag (string-trim tag)) tokens)))]
               [else
                (loop (cons (make-tmpl-token 'text "{") tokens))]))]
          [else
           (let ([text (read-plain-text port)])
             (if (string=? text "")
                 (loop tokens)
                 (loop (cons (make-tmpl-token 'text text) tokens))))]))))

  ;; Read plain text until '{' or EOF
  (define (read-plain-text port)
    (let loop ([chars '()])
      (let ([c (peek-char port)])
        (cond
          [(eof-object? c) (list->string (reverse chars))]
          [(char=? c #\{) (list->string (reverse chars))]
          [else
           (read-char port)
           (loop (cons c chars))]))))

  ;; Read until delimiter string
  (define (read-until-delim port delim)
    (let ([d0 (string-ref delim 0)]
          [d1 (string-ref delim 1)])
      (let loop ([chars '()])
        (let ([c (read-char port)])
          (cond
            [(eof-object? c)
             (list->string (reverse chars))]
            [(and (char=? c d0)
                  (not (eof-object? (peek-char port)))
                  (char=? (peek-char port) d1))
             (read-char port)  ; consume second delimiter char
             (list->string (reverse chars))]
            [else
             (loop (cons c chars))])))))

  ;; Render list of tokens
  (define (render-tokens tokens context out)
    (let loop ([tokens tokens])
      (unless (null? tokens)
        (let ([token (car tokens)])
          (case (tmpl-token-type token)
            [(text)
             (display (tmpl-token-value token) out)
             (loop (cdr tokens))]
            [(expr)
             (let ([value (eval-template-expr (tmpl-token-value token) context)])
               (when value
                 (display (format-value value) out)))
             (loop (cdr tokens))]
            [(tag)
             (let ([remaining (handle-template-tag
                                (tmpl-token-value token)
                                (cdr tokens)
                                context
                                out)])
               (loop remaining))])))))

  (define (format-value val)
    (cond
      [(string? val) val]
      [(number? val) (number->string val)]
      [(symbol? val) (symbol->string val)]
      [(boolean? val) (if val "true" "false")]
      [else (format "~a" val)]))

  ;; Handle control tags
  (define (handle-template-tag tag-str tokens context out)
    (let ([parts (remp (lambda (s) (string=? s ""))
                       (string-split tag-str " "))])
      (if (null? parts)
          tokens
          (let ([tag-name (car parts)])
            (cond
              [(string=? tag-name "if")
               (handle-if-tag (cdr parts) tokens context out)]
              [(string=? tag-name "for")
               (handle-for-tag (cdr parts) tokens context out)]
              [(or (string=? tag-name "endif")
                   (string=? tag-name "endfor"))
               tokens]
              [else
               (display (format "<!-- Unknown tag: ~a -->" tag-str) out)
               tokens])))))

  ;; Handle {% if condition %}...{% endif %}
  (define (handle-if-tag args tokens context out)
    (if (null? args)
        tokens
        (let* ([condition (car args)]
               [body-and-rest (collect-block-tokens tokens "endif")]
               [body-tokens (car body-and-rest)]
               [remaining (cdr body-and-rest)]
               [value (eval-template-expr condition context)])
          (when value
            (render-tokens body-tokens context out))
          remaining)))

  ;; Handle {% for item in list %}...{% endfor %}
  (define (handle-for-tag args tokens context out)
    (if (< (length args) 3)
        tokens
        (let* ([var-name (car args)]
               ;; args = (var "in" list-expr)
               [list-expr (caddr args)]
               [body-and-rest (collect-block-tokens tokens "endfor")]
               [body-tokens (car body-and-rest)]
               [remaining (cdr body-and-rest)]
               [list-value (eval-template-expr list-expr context)])
          (when list-value
            (let ([items (cond
                           [(vector? list-value) (vector->list list-value)]
                           [(list? list-value) list-value]
                           [else '()])])
              (for-each
                (lambda (item)
                  (let ([new-context (cons (cons (string->symbol var-name) item)
                                          context)])
                    (render-tokens body-tokens new-context out)))
                items)))
          remaining)))

  ;; Collect tokens until matching end tag
  (define (collect-block-tokens tokens end-tag)
    (let loop ([tokens tokens] [collected '()] [depth 0])
      (if (null? tokens)
          (cons (reverse collected) '())
          (let ([token (car tokens)])
            (if (eq? (tmpl-token-type token) 'tag)
                (let* ([tag-str (tmpl-token-value token)]
                       [parts (remp (lambda (s) (string=? s ""))
                                   (string-split tag-str " "))]
                       [tag-name (if (null? parts) "" (car parts))])
                  (cond
                    [(and (string=? tag-name end-tag) (= depth 0))
                     (cons (reverse collected) (cdr tokens))]
                    [(or (string=? tag-name "if") (string=? tag-name "for"))
                     (loop (cdr tokens) (cons token collected) (+ depth 1))]
                    [(or (string=? tag-name "endif") (string=? tag-name "endfor"))
                     (loop (cdr tokens) (cons token collected) (max 0 (- depth 1)))]
                    [else
                     (loop (cdr tokens) (cons token collected) depth)]))
                (loop (cdr tokens) (cons token collected) depth))))))

  ;; Evaluate expression like "user.name" in context
  (define (eval-template-expr expr context)
    (let ([parts (string-split (string-trim expr) ".")])
      (let loop ([parts parts] [value context])
        (if (null? parts)
            value
            (let* ([part (string-trim (car parts))]
                   [key (string->symbol part)])
              (cond
                [(list? value)
                 (let ([pair (assq key value)])
                   (if pair
                       (loop (cdr parts) (cdr pair))
                       #f))]
                [else #f]))))))
)
