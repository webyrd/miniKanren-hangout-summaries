Chez Scheme Version 9.4.1
Copyright 1984-2016 Cisco Systems, Inc.

> (load "hangout-7.scm")
> 5
5
> (+ 3 4)
7
> ==
#<procedure == at mk.scm:12262>
> 

Process scheme finished
Chez Scheme Version 9.4.1
Copyright 1984-2016 Cisco Systems, Inc.

> ==

Exception: variable == is not bound
Type (debug) to enter the debugger.
> (load "hangout-7.scm")
> ==
#<procedure == at mk.scm:12262>
> (= 5 5)
#t
> (= 5 6)
#f
> (equal? 5 5)
#t
> (equal? '(a b c) '(a b c))
#t
> (equal? '(a ((b)) c) '(a ((b)) c))
#t
> (equal? '(a ((b)) c) '(a ((e)) c))
#f
> (== 6 6)
#<procedure at mk.scm:12280>
> ==
#<procedure == at mk.scm:12262>
> run

Exception: invalid syntax run
Type (debug) to enter the debugger.
> (run 1 (q) (== 6 6))
(_.0)
> (run 1 (q) (== 5 6))
()
> (run 1 (q) (== q 6))
(6)
> (run 1 (q) (== (list 3 4) (list 3 4)))
(_.0)
> (run 1 (q) (== '(3 4) (list 3 4)))
(_.0)
> (list 3 4)
(3 4)
> '(3 4)
(3 4)
> (equal? '(3 4) (list 3 4))
#t
> (run 1 (q) (== (list 3 4) (list q 4)))
(3)
> (run 1 (x y)
    (== (list 3 y)
        (list x 4)))
((3 4))
> (run 1 (x y)
    (== (list 3 4)
        (list x y)))
((3 4))
> (run 1 (x y)
    (== (list x y)
        (list 3 4)))
((3 4))
> (run 1 (q) (== 5 6))
()
> (run 1 (q)
    (== (list 3 4)
        (list q q)))
()
> (run 1 (q)
    (== (list 3 3)
        (list q q)))
(3)
> (run 1 (q)
    (== (list '(a b c) '(a b c))
        (list q q)))
((a b c))
> (run 1 (q)
    (fresh ()
      (== q 5)))
(5)
> (run 1 (q)
    (== q 5))
(5)
> (run 1 (q)
    (fresh (x)
      (== x 5)))
(_.0)
> (run 1 (q)
    (fresh (x)
      (== x 5)
      (== q x)))
(5)
> (run 1 (q)
    (fresh (x)
      (== q x)
      (== x 5)))
(5)
> (run 1 (q)
    (fresh (x)
      (== q x)))
(_.0)
> (run 1 (x y) (== x y))
((_.0 _.0))
> (run 1 (x y) (== 5 5))
((_.0 _.1))
> (run 1 (x y) (== x y) (== x 5))
((5 5))
> (run 1 (x y) (== x y) (== y 5))
((5 5))
> (run 1 (x y) (== y 5) (== x y))
((5 5))
> (run 1 (x y)
    (== x y)
    (== y 5)
    (== x 5))
((5 5))
> (run 1 (x y)
    (== x y)
    (== y 5)
    (== x 6))
()
> (run 1 (x y)
    (== y 5)
    (== x 6))
((6 5))
> (run 1 (x y)
    (== x y)
    (== y 5)
    (== x 6))
()
> (run 1 (x y)
    (== y 5) ; y == 5
    (== x 6) ; x == 6, y == 5
    (== x y))
()
> (run 1 (x y)
    (fresh ()
      (== x y)
      (== y 5)
      (== x 6)))
()
> (run 1 (x y)
    (fresh ()
      (== y 5)
      (== x 6)))
((6 5))
> (run 1 (x y)
    (== y 5)
    (== x 6)
    (== x y))
()
> (conde
    [(== x 5) (== y 7)]
    [(== x 6)])
#<procedure>
> (fresh (x) (== x 5))
#<procedure>
> (== x 5)

Exception: variable x is not bound
Type (debug) to enter the debugger.
> (== 5 5)
#<procedure at mk.scm:12280>
> (run 1 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5)
> (run 1 (q)
    (== q 5))
(5)
> (run 2 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run 1 (q)
    (== q 6))
(6)
> (run 2 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run 2 (q)
    (conde
      [(== 7 5)]
      [(== q 6)]))
(6)
> (run 2 (q)
    (conde
      [(== 7 5)]
      [(== 4 6)]))
()
> (run 2 (q)
    (conde
      [(== 5 5)]
      [(== 4 6)]))
(_.0)
> (run 2 (q)
    (conde
      [(== 5 5)]
      [(== 4 4)]))
(_.0 _.0)
> (run 2 (q)
    (conde
      [(== 4 4)]))
(_.0)
> (run 2 (q)
    (conde
      [(== q 5)]
      [(== q 4)]))
(5 4)
> (run 1 (q)
    (conde
      [(== q 5)]
      [(== q 4)]))
(5)
> (run 3 (q)
    (conde
      [(== q 5)]
      [(== q 4)]))
(5 4)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 4)]))
(5 4)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 4)]
      [(== q 6)]
      [(== q 7)]))
(5 4 6 7)
> (run* (x y)
    (conde
      [(== x 5) (== y 1)]
      [(== x 4) (== y 2)]
      [(== x 6) (== y 3)]
      [(== x 7) (== y 4)]))
((5 1) (4 2) (6 3) (7 4))
> (run* (x y)
    (conde
      [(== x 5) (== y 1)]
      [(== x 4)]
      [(== y 3)]
      [(== x 7) (== y 4)]))
((5 1) (4 _.0) (_.0 3) (7 4))
> (run* (q)
    (fresh (x y)
      (fresh (z)
        (== z 5)
        (== y q))
      (== y 6)))
(6)
> (run* (q)
    (fresh (x y)
      (fresh (z)
        (== x 5)
        (== y q))
      (== z 6)))

Exception: variable z is not bound
Type (debug) to enter the debugger.
> (run* (q)
    (fresh (x y)
      (fresh (q)
        (== q 7))
      (== x 6)))
(_.0)
> (run* (q)
    (fresh (x y)
      (fresh (foo)
        (== foo 7))
      (== x 6)))
(_.0)
> (run* (q)
    (fresh (x y)
      (fresh (foo)
        (conde
          [(fresh (z)
             (== q z))
           (== 5 5)]
          [(conde
             [(== q 7)])]))
      (== x 6)))
(_.0 7)
> (run* (q)
    (fresh (x y)
      (fresh (foo)
        (conde
          [(fresh (z)
             (== q z))
           (== 5 5)]
          [(conde
             [(== q 7)])]))
      (== (== x 6) y)))
(_.0 7)
> (run* (q)
    (fresh (x y)
      (fresh (foo)
        (conde
          [(fresh (z)
             (== q z))
           (== 5 5)]
          [(conde
             [(== q 7)])]))
      (== (== x 6) y)))
(_.0 7)
> (run 1 (q)
    (fresh (x)
      (== x 5)))
(_.0)
> (run 1 (q)
    (fresh (x)
      (== x 5)
      (== x q)))
(5)
> (run 1 (q)
    (fresh (x)
      (== (== x 5) q)))
(#<procedure at mk.scm:12280>)
> (run 1 (q) (== q 5))
(5)
> (run 1 (q) (== q '(a b c)))
((a b c))
> (run 1 (q) (== '(a b c) q))
((a b c))
> (run 1 (q) (== q '(a b c)))
((a b c))
> (run 1 (q) (== q '(a b c)) (== q 5))
()
> (run 1 (q)
    (conde
      [(== q '(a b c))]
      [(== q 5)]))
((a b c))
> (run 2 (q)
    (conde
      [(== q '(a b c))]
      [(== q 5)]))
((a b c) 5)
> (run 1 (q) (== q '(a b c)))
((a b c))
> (run 1 (q) (== (list q) q))
()
> ==, fresh, conde            run, run*