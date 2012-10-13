(use debug
     srfi-13
     test)

(test "Trivial example"
      "((x 2) (y #<procedure (y . x)>))\n"
      (with-output-to-string
        (lambda ()
          (set! current-error-port current-output-port)
          (let ((x 2)
                (y (lambda x x)))
            (debug x y)))))

(test "Catching error"
      "(((car '() '())\n  \"Error: bad argument count - received 2 but expected 1: #<procedure (car p)>\")\n ((string-join 12)\n  \"Error: STRINGS parameter not list.: 12, #<procedure (string-join strings2036 . delim+grammar2037)>\")\n ((error \"For reals?\") \"Error: For reals?\"))\n"
      (with-output-to-string
        (lambda ()
          (parameterize ((current-error-port (current-output-port)))
            (debug (car '() '())
                   (string-join 12)
                   (error "For reals?"))))))

(test "Debugging-off"
      ""
      (parameterize ((debug? #f)
                     (current-error-port (current-output-port)))
        (with-output-to-string
          (lambda ()
            (let ((x 2)
                  (y (lambda x x)))
              (debug x y))))))

(test "Trace"
      ";; Arguments to x: ()\n;; Values from x: (2)\n(((x) 2))\n"
      (begin
        (define (x) 2)
        (trace x)
        (with-output-to-string
          (lambda ()
            (parameterize ((current-error-port (current-output-port)))
              (debug (x)))))))

(test-exit)
