@(title "Debug")
@(description "A couple of Joe-Armstrong-style debugging macros")
@(author "Peter Danenberg")
@(username "klutometis")
@(email "pcd@roxygen.org")

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
           (pp `((x ,(handle-exceptions
                      exn
                      (format "Error: ~a; arguments: ~a"
                              ((condition-property-accessor
                                'exn
                                'message)
                               exn)
                              (string-join
                               (map ->string
                                    ((condition-property-accessor
                                      'exn
                                      'arguments)
                                     exn))
                               ", "))
                      x))
                 ...))))))))

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
       void
       (lambda () (syslog priority buffer)))))))

(define-syntax debug/syslog
  @("Debug to syslog."
    (expressions "The expressions to debug (cf. `debug' supra)"))
  (er-macro-transformer
   (lambda (expression rename compare)
     `(let ((port (make-syslog-port)))
        (with-error-output-to-port
         port
         (lambda ()
           (when (debug?)
             (debug ,@(cdr expression))
             (flush-output port))))))))
