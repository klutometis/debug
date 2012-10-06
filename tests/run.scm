(use debug
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
      "(((car '()) \"bad argument type\"))\n"
      (with-output-to-string
        (lambda ()
          (parameterize ((current-error-port (current-output-port)))
            (debug (car '()))))))

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
