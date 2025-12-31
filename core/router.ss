;;; core/router.ss - URL routing with pattern matching

(library (core router)
  (export
    make-router
    router?
    router-add-route!
    router-match
    router-get!
    router-post!
    router-put!
    router-delete!
    router-patch!
    router-options!
    router-head!
    router-any!)

  (import (chezscheme)
          (utils helpers))

  ;; Route record
  (define-record-type route
    (fields
      method      ; Symbol: GET, POST, PUT, DELETE, PATCH, OPTIONS, HEAD, or ANY
      pattern     ; Original pattern string
      segments    ; Parsed segments (strings or symbols for params)
      handler))   ; Handler function: (request params) -> response

  ;; Router record with protocol
  (define-record-type router
    (fields
      (mutable routes))
    (protocol
      (lambda (new)
        (lambda ()
          (new '())))))

  ;; Parse path pattern into segments
  ;; "/users/<id>/posts/<post_id>" -> ("users" id "posts" post_id)
  ;; Supports: <param> for parameters, * for wildcard
  (define (parse-pattern pattern)
    (let* ([trimmed (cond
                     [(string=? pattern "") ""]
                     [(string-prefix? "/" pattern)
                      (substring pattern 1 (string-length pattern))]
                     [else pattern])])
      (if (string=? trimmed "")
          '()
          (map (lambda (part)
                 (cond
                   ;; Parameter: <name>
                   [(and (> (string-length part) 2)
                         (char=? (string-ref part 0) #\<)
                         (char=? (string-ref part (- (string-length part) 1)) #\>))
                    (string->symbol
                      (substring part 1 (- (string-length part) 1)))]
                   ;; Wildcard
                   [(string=? part "*")
                    '*]
                   ;; Literal
                   [else part]))
               (string-split trimmed "/")))))

  ;; Add a route
  (define (router-add-route! rtr method pattern handler)
    (let* ([segments (parse-pattern pattern)]
           [rt (make-route method pattern segments handler)])
      (router-routes-set! rtr
                          (append (router-routes rtr) (list rt)))))

  ;; Convenience methods
  (define (router-get! rtr pattern handler)
    (router-add-route! rtr 'GET pattern handler))

  (define (router-post! rtr pattern handler)
    (router-add-route! rtr 'POST pattern handler))

  (define (router-put! rtr pattern handler)
    (router-add-route! rtr 'PUT pattern handler))

  (define (router-delete! rtr pattern handler)
    (router-add-route! rtr 'DELETE pattern handler))

  (define (router-patch! rtr pattern handler)
    (router-add-route! rtr 'PATCH pattern handler))

  (define (router-options! rtr pattern handler)
    (router-add-route! rtr 'OPTIONS pattern handler))

  (define (router-head! rtr pattern handler)
    (router-add-route! rtr 'HEAD pattern handler))

  (define (router-any! rtr pattern handler)
    (router-add-route! rtr 'ANY pattern handler))

  ;; Match a request against routes
  ;; Returns (handler . params) or #f
  (define (router-match rtr method path)
    (let ([path-segments (parse-request-path path)])
      (let loop ([routes (router-routes rtr)])
        (if (null? routes)
            #f
            (let* ([rt (car routes)]
                   [match-result (try-match-route rt method path-segments)])
              (if match-result
                  (cons (route-handler rt) match-result)
                  (loop (cdr routes))))))))

  ;; Parse request path into segments
  (define (parse-request-path path)
    (let* ([path-only (let ([q-pos (string-index path #\?)])
                       (if q-pos
                           (substring path 0 q-pos)
                           path))]
           [trimmed (if (and (> (string-length path-only) 0)
                            (char=? (string-ref path-only 0) #\/))
                       (substring path-only 1 (string-length path-only))
                       path-only)])
      (if (string=? trimmed "")
          '()
          (string-split trimmed "/"))))

  ;; Try to match a single route
  ;; Returns params alist or #f
  (define (try-match-route rt method path-segments)
    (let ([route-method (route-method rt)]
          [route-segments (route-segments rt)])
      (and (or (eq? route-method 'ANY)
               (eq? route-method method))
           (match-segments route-segments path-segments))))

  ;; Match segments and extract params
  ;; Returns params alist or #f
  (define (match-segments route-segments path-segments)
    (let loop ([route-segs route-segments]
               [path-segs path-segments]
               [params '()])
      (cond
        ;; Both empty - match!
        [(and (null? route-segs) (null? path-segs))
         (reverse params)]

        ;; Route empty but path not - no match
        [(null? route-segs)
         #f]

        ;; Path empty but route not - no match
        [(null? path-segs)
         #f]

        ;; Wildcard matches rest of path
        [(eq? (car route-segs) '*)
         (reverse (cons (cons '* (string-join path-segs "/")) params))]

        ;; Parameter - capture value
        [(symbol? (car route-segs))
         (loop (cdr route-segs)
               (cdr path-segs)
               (cons (cons (car route-segs) (car path-segs)) params))]

        ;; Literal - must match exactly
        [(string=? (car route-segs) (car path-segs))
         (loop (cdr route-segs)
               (cdr path-segs)
               params)]

        ;; No match
        [else #f])))
)
