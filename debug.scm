(module
 debug
 (debug)
 (define-syntax debug
  (syntax-rules ()
    ((_ x ...)
     (print `((x ,x) ...))))))
