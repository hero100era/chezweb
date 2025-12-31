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
    <li><a href='/forms'>Form Submission Example</a></li>
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

;; ----- Form Handling -----

;; Form page
(app-get app "/forms"
  (lambda (req)
    (html "
<!DOCTYPE html>
<html>
<head>
  <title>Chez Web Form Example</title>
  <style>
    body { font-family: -apple-system, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; line-height: 1.6; }
    h1 { color: #333; }
    .form-group { margin-bottom: 20px; border: 1px solid #ddd; padding: 15px; border-radius: 5px; }
    label { display: block; margin-bottom: 5px; font-weight: bold; }
    input[type='text'], input[type='file'] { width: 100%; padding: 8px; margin-bottom: 10px; box-sizing: border-box; }
    input[type='submit'] { background: #0066cc; color: white; border: none; padding: 10px 20px; border-radius: 3px; cursor: pointer; }
    input[type='submit']:hover { background: #0052a3; }
  </style>
</head>
<body>
  <h1>Form Submission Example</h1>
  
  <div class='form-group'>
    <h2>URL Encoded Form</h2>
    <form action='/submit-form' method='POST'>
      <label for='name'>Name:</label>
      <input type='text' id='name' name='name' placeholder='Enter your name'>
      
      <label for='email'>Email:</label>
      <input type='text' id='email' name='email' placeholder='Enter your email'>
      
      <input type='submit' value='Submit URL Encoded'>
    </form>
  </div>

  <div class='form-group'>
    <h2>Multipart Form (with File Upload)</h2>
    <form action='/upload' method='POST' enctype='multipart/form-data'>
      <label for='username'>Username:</label>
      <input type='text' id='username' name='username' placeholder='Choose a username'>
      
      <label for='profile_pic'>Profile Picture:</label>
      <input type='file' id='profile_pic' name='profile_pic'>
      
      <input type='submit' value='Upload Multipart'>
    </form>
  </div>
  <p><a href='/'>Back to home</a></p>
</body>
</html>")))

;; Handle URL encoded form
(app-post app "/submit-form"
  (lambda (req)
    (let* ([form (request-form req)]
           [name (assoc-ref form "name")]
           [email (assoc-ref form "email")])
      (html (format "
<html>
<body>
  <h1>Form Received!</h1>
  <p><strong>Name:</strong> ~a</p>
  <p><strong>Email:</strong> ~a</p>
  <hr>
  <p>Full Form Data: ~s</p>
  <p><a href='/forms'>Back</a></p>
</body>
</html>" name email form)))))

;; Handle Multipart form and file upload
(app-post app "/upload"
  (lambda (req)
    (let* ([form (request-form req)]
           [files (request-files req)]
           [username (assoc-ref form "username")]
           [pic-info (assoc-ref files "profile_pic")])
      (html (format "
<html>
<body>
  <h1>Upload Received!</h1>
  <p><strong>Username:</strong> ~a</p>
  <hr>
  <h2>File Information:</h2>
  ~a
  <hr>
  <p><a href='/forms'>Back</a></p>
</body>
</html>" 
             username
             (if pic-info
                 (format "<ul>
                            <li><strong>Filename:</strong> ~a</li>
                            <li><strong>Content-Type:</strong> ~a</li>
                            <li><strong>Size:</strong> ~a bytes</li>
                          </ul>"
                         (assoc-ref pic-info 'filename)
                         (assoc-ref pic-info 'content-type)
                         (string-length (assoc-ref pic-info 'data)))
                 "<p>No file uploaded</p>"))))))

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
