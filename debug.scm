(module
 debug
 (debug)
 (import chicken scheme)
 (define-syntax debug
  (syntax-rules ()
    ((_ x ...)
     (display `((x ,x) ...) (current-error-port))))))
