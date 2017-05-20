(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")

(define binary-conde
  (lambda ()
    (fresh (x)
      (conde
        [(== x 1)]
        [(== x 2)]))))

(define run-degenerate-binary-condes-tests
  (lambda ()
  
    (printf "degenerate binary condes tests...\n")

    (time
     (test "degenerate-binary-condes-1a"
       (run 1 (q)
         (binary-conde))
       '(_.0)))

    (time
     (test "degenerate-binary-condes-1b"
       (run* (q)
         (binary-conde))
       '(_.0 _.0)))

    (time
     (test "degenerate-binary-condes-10a"
       (run 1 (q)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde))
       '(_.0)))

    (time
     (test "degenerate-binary-condes-10b"
       (length
        (run* (q)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)
          (binary-conde)))
       1024))

    (time
     (test "degenerate-binary-condes-20a"
       (run 1 (q)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)
         (binary-conde)           
         )
       '(_.0)))
      
    ))

#!eof





(time
 (run 1 (q)
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   (fresh (x)
     (conde
       [(== x 1)]
       [(== x 2)]))
   ))
