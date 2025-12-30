#!/usr/bin/env scheme-script
;;; examples/app.ss - Example web application

(import (chezscheme)
        (chezweb))

;; Create the application
(define app (make-app))

;; ----- Middleware -----

;; Enable CORS - use alist style options
(app-use app (make-cors-middleware
               '((origin . "*")
                 (methods . "GET, POST, PUT, DELETE, OPTIONS"))))

;; ----- Routes -----

;; Home page
(app-get app "/"
  (lambda (req)
    (html "
<!DOCTYPE html>
<html>
<head>
  <title>Chez Web</title>
  <style>
    body { font-family: -apple-system, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
    h1 { color: #333; }
    a { color: #0066cc; }
    ul { line-height: 2; }
    code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
  </style>
</head>
<body>
  <h1>Welcome to Chez Web!</h1>
  <p>A Flask-like web framework for Chez Scheme</p>

  <h2>Example Routes:</h2>
  <ul>
    <li><a href='/hello/World'>Hello with parameter</a></li>
    <li><a href='/api/users'>Users API (JSON)</a></li>
    <li><a href='/api/users/42'>Single User</a></li>
  </ul>
</body>
</html>")))

;; Hello with URL parameter
(app-get app "/hello/<name>"
  (lambda (req)
    (let ([name (assoc-ref (request-params req) 'name)])
      (html (format "
<!DOCTYPE html>
<html>
<head><title>Hello</title></head>
<body>
  <h1>Hello, ~a!</h1>
  <p><a href='/'>Back to home</a></p>
</body>
</html>" name)))))

;; ----- JSON API -----

;; Get all users
(app-get app "/api/users"
  (lambda (req)
    (json-response
      `((users . #(((id . 1) (name . "Alice"))
                   ((id . 2) (name . "Bob"))
                   ((id . 3) (name . "Charlie"))))
        (total . 3)))))

;; Get single user
(app-get app "/api/users/<id>"
  (lambda (req)
    (let ([id (assoc-ref (request-params req) 'id)])
      (json-response
        `((id . ,(string->number id))
          (name . ,(format "User ~a" id))
          (email . ,(format "user~a@example.com" id)))))))

;; Create user (POST)
(app-post app "/api/users"
  (lambda (req)
    (let ([body (request-json-body req)])
      (if body
          (json-response
            `((message . "User created")
              (user . ,body))
            201)
          (json-response
            `((error . "Invalid JSON"))
            400)))))

;; ----- Run the Application -----

(display "\n")
(display "========================================\n")
(display "   Chez Web Example Application\n")
(display "========================================\n")

;; Use alist for options
(app-run app '((host . "127.0.0.1")
               (port . 5000)
               (debug . #t)))
