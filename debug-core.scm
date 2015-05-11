@(heading "Abstract")
@(text "According to [[http://books.google.com/books?id=nneBa6-mWfgC&lpg=PA227&ots=gEvyGdNW3u&dq=%22thou%20shalt%20put%20printf%22&pg=PA227#v=onepage&q=%22thou%20shalt%20put%20printf%22&f=false|Joe Armstrong]], \"The great gods of programming said, 'Thou shalt put {{printf}}
statements in your program at the point where you think it’s gone
wrong, recompile, and run it.'\"")

@(heading "Documentation")
(define debug?
  @("`debug?' turns on or off debugging output, depending on whether
it is set to #t or #f; respectively.")
  (make-parameter #t))

(define-syntax trace
  @("Trace the input to and output from a function."
    (f "The function to be traced"))
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
             (%f (rename 'f))
             (%when (rename 'when))
             (%debug? (rename 'debug?)))
         `(,%when
           (,%debug?)
           (,%let ((,%f ,f))
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
                               (,%apply ,%values return-values)))))))))))

(define-syntax debug
  @("Debug the expressions to stderr by pretty-printing each
expression and their evaluations."
    (expressions "The expressions to be debugged"))
  (syntax-rules ()
    ((_ x ...)
     (with-output-to-port
         (current-error-port)
       (lambda ()
         (when (debug?)
           ;; Handle some of the self-evaluating scalars in an ad-hoc
           ;; fashion (can't do this with e.g. lists and symbols,
           ;; though); alternatively: compare the expression with its
           ;; evaluated form for equality over some arbitrary
           ;; predicate (e.g. `equal?'): this might imply
           ;; self-evaluation.
           (pp `(,(if (or (boolean? 'x)
                          (char? 'x)
                          (number? 'x)
                          (string? 'x)
                          (vector? 'x)
                          (and (pair? 'x) (eq? (car 'x) 'quote)))
                      x
                      `(x =>
                          ,(handle-exceptions
                               exn
                             (let ((message
                                    ((condition-property-accessor
                                      'exn
                                      'message)
                                     exn))
                                   (arguments
                                    ((condition-property-accessor
                                      'exn
                                      'arguments)
                                     exn)))
                               (format "Error: ~a~a"
                                       message
                                       (if (null? arguments)
                                           ""
                                           (format
                                            ": ~a"
                                            (string-join
                                             (map ->string arguments)
                                             ", ")))))
                             x)))
                 ...))))))))

(define debug-priority
  @("The priority associated with {{debug/syslog}}")
  (make-parameter prio/debug))

(define make-syslog-port
  (case-lambda
   (()
    (make-syslog-port (debug-priority)))
   ((priority)
    (let ((buffer ""))
      (make-output-port
       (lambda (scribendum)
         (set! buffer (string-append/shared buffer scribendum)))
       void
       (lambda () (syslog priority buffer)))))))

(define-syntax debug/syslog
  @("Debug to syslog."
    (expressions "The expressions to debug (cf. `debug' supra)"))
  (ir-macro-transformer
   (lambda (expression rename inject)
     `(let ((port (make-syslog-port)))
        (with-error-output-to-port
         port
         (lambda ()
           (when (debug?)
             (debug ,@(cdr expression))
             (flush-output port))))))))
