(module
 debug
 (debug debug/syslog make-syslog-port trace)
 (import chicken scheme extras data-structures ports)
 (import-for-syntax ports matchable)
 (use syslog srfi-13)

 (define-syntax trace
   (er-macro-transformer
    (lambda (expression rename compare) 
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
      (with-output-to-port
          (current-error-port)
        (lambda ()
          (write `((x ,x) ...))
          (newline))))))

 (define default-priority (make-parameter prio/debug))
 
 (define make-syslog-port
   (case-lambda
    (()
     (make-syslog-port (default-priority)))
    ((priority)
     (let ((buffer ""))
       (make-output-port
        (lambda (scribendum)
          (set! buffer (string-append/shared buffer scribendum)))
        noop
        (lambda () (syslog priority buffer)))))))
    
 (define-syntax debug/syslog
   (er-macro-transformer
    (lambda (expression rename compare)
      `(let ((port (make-syslog-port)))
         (with-error-output-to-port
          port
          (lambda ()
            (debug ,@(cdr expression))
            (flush-output port))))))))
