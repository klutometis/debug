(module
 debug
 (debug)
 (import chicken scheme)
 (define-syntax debug
  (syntax-rules ()
    ((_ x ...)
     (begin (display `((x ,x) ...) (current-error-port))
            (display #\newline (current-error-port)))))))
