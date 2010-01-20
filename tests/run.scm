(use debug
     test)

(test "trivial example"
      "((x 2) (y #<procedure (y . x)>))\n"
      (with-output-to-string
        (lambda ()
          (let ((x 2)
                (y (lambda x x)))
            (debug x y)))))
