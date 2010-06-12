(module
 debug
 (debug trace)
 (import chicken scheme)

 (define-syntax trace
   (er-macro-transformer
    (lambda (expression rename compare)
      (import matchable)
      (match-let (((_ f) expression))
        (let ((%set! (rename 'set!))
              (%lambda (rename 'lambda))
              (%call-with-values (rename 'call-with-values))
              (%apply (rename 'apply))
              (%format (rename 'format))
              (%values (rename 'values))
              (%let (rename 'let))
              (%f (rename 'f)))
          `(,%let ((,%f ,f))
             (,%set!
              ,f
              (,%lambda x
                (,%format (current-error-port)
                          ";; Arguments to ~a: ~a~%"
                          ',f
                          x)
                (,%let ((return-values
                         (,%call-with-values
                             (,%lambda () (,%apply ,%f x))
                           (,%lambda x x))))
                  (,%format (current-error-port)
                            ";; Values from ~a: ~a~%"
                            ',f
                            return-values)
                  (,%apply ,%values return-values))))))))))

 (define-syntax debug
  (syntax-rules ()
    ((_ x ...)
     (begin (display `((x ,x) ...) (current-error-port))
            (display #\newline (current-error-port)))))))
