;;; utils/multipart.ss - Multipart form-data parsing

(library (utils multipart)
  (export
    parse-multipart-data)

  (import (chezscheme)
          (utils helpers))

  ;; Parse multipart form data
  ;; Returns a list of (name . value) or (name . ((filename . "...") (content-type . "...") (data . "...")))
  (define (parse-multipart-data body boundary)
    (let* ([boundary-str (string-append "--" boundary)]
           [parts (string-split body boundary-str)])
      (let loop ([parts parts] [results '()])
        (if (null? parts)
            (reverse results)
            (let ([part (string-trim (car parts))])
              (if (or (string=? part "") (string=? part "--"))
                  (loop (cdr parts) results)
                  (let* ([header-end (string-contains part "\r\n\r\n")]
                         [header-section (if header-end
                                            (substring part 0 header-end)
                                            part)]
                         [part-body (if header-end
                                       (substring part (+ header-end 4) (string-length part))
                                       "")]
                         [headers (parse-multipart-headers header-section)]
                         [disposition (assoc-ref headers "content-disposition")])
                    (if disposition
                        (let* ([name (extract-param disposition "name")]
                               [filename (extract-param disposition "filename")])
                          (if filename
                              (loop (cdr parts)
                                    (cons (cons name 
                                                `((filename . ,filename)
                                                  (content-type . ,(assoc-ref headers "content-type"))
                                                  (data . ,part-body)))
                                          results))
                              (loop (cdr parts)
                                    (cons (cons name part-body) results))))
                        (loop (cdr parts) results)))))))))

  ;; Parse headers for a single part
  (define (parse-multipart-headers section)
    (let ([lines (string-split section "\r\n")])
      (map (lambda (line)
             (let ([colon (string-index line #\:)])
               (if colon
                   (cons (string-downcase (string-trim (substring line 0 colon)))
                         (string-trim (substring line (+ colon 1) (string-length line))))
                   (cons "" ""))))
           lines)))

  ;; Extract parameter value from header (e.g., name="foo" from Content-Disposition)
  (define (extract-param header param-name)
    (let* ([search-str (string-append param-name "=\"")]
           [start (string-contains header search-str)])
      (if start
          (let* ([val-start (+ start (string-length search-str))]
                 [remaining (substring header val-start (string-length header))]
                 [end (string-index remaining #\")])
            (if end
                (substring remaining 0 end)
                #f))
          #f)))

)
