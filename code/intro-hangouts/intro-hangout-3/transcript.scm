Chez Scheme Version 9.4.1
Copyright 1984-2016 Cisco Systems, Inc.

> `(a ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> `(a (d ,(car '(e f))))
(a (d e))
> (car '(e f))
e
> (quasiquote
   (a (quote (d (unquote (car (quote (e f))))))))
(a '(d e))
> (quasiquote
   (a (quote (d (unquote (car (quote (e f))))))))
(a '(d e))
> (quote (d (unquote (car (quote (e f))))))
(d ,(car '(e f)))
> `(a '(d ,(car '(e f))))
(a '(d e))
> `(a ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> `(a ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> `(a ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> `(a ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> 
'(a
  ,(append `(cdr '(a b c)) '())
  '(d ,(car '(e f))))
(a
 ,(append `(cdr '(a b c)) '())
 '(d ,(car '(e f))))
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> (append `(cdr '(a b c)) '())
(cdr '(a b c))
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> `(a
    ,(append `(cdr '(a b c)) '(d ,(car '(e f)))))
(a (cdr '(a b c) d ,(car '(e f))))
> (append `(cdr '(a b c)) '(d ,(car '(e f))))
(cdr '(a b c) d ,(car '(e f)))
> `(car '(a b c))
(car '(a b c))
> `,(car '(a b c))
a
> (car '(a b c))
a
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> (car '(e f))
e
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> `(a
    ,(append `(cdr '(a b c)) '())
    '(d ,(car '(e f))))
(a (cdr '(a b c)) '(d e))
> (quasiquote
   (a
    (unquote (append (quasiquote
                      (cdr (quote (a b c))))
                     (quote ())))
    (quote (d (unquote (car (quote (e f))))))))
(a (cdr '(a b c)) '(d e))
> (quasiquote
   (a
    (unquote (append (quasiquote
                      (cdr (quote (a b c))))
                     (quote ())))
    (quote (d (unquote (car (quote (e f))))))))
(a (cdr '(a b c)) '(d e))
> (append (quasiquote
                      (cdr (quote (a b c))))
                     (quote ()))
(cdr '(a b c))
> (car (quote (e f)))
e
> `(a b '('('('('('(c ,(add1 3) f)))))) d)
(a b '('('('('('(c 4 f)))))) d)
> (list 'a 'b)
(a b)
> (let ((e1 (list 'a 'b))
        (e2 (list 'c (add1 3) 'd)))
    (list e1 e2))
((a b) (c 4 d))
> (let ((x 7))
    (* x x))
49
> ((lambda (x) (* x x)) 7)
49
> 49
49
> (define my-let
    (syntax-rules ()
      ((my-let ((x e)) body)
       ((lambda (x) body) e))))
> (my-let ((x 7))
    (* x x))

Exception: variable x is not bound
Type (debug) to enter the debugger.
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e)) body)
       ((lambda (x) body) e))))
> (my-let ((x 7))
    (* x x))
49
> ((lambda (x) (* x x)) 7)
49
> (let ((x 7))
    (* x x))
49
> (expand '((lambda (x) (* x x)) 7))
(let ([#{x pfv8xpi8rnhlin5jm8f58ltkq-0} 7])
  (#2%*
    #{x pfv8xpi8rnhlin5jm8f58ltkq-0}
    #{x pfv8xpi8rnhlin5jm8f58ltkq-0}))
> (expand '(let ((x 7))
    (* x x)))
(let ([#{x pfv8xpi8rnhlin5jm8f58ltkq-1} 7])
  (#2%*
    #{x pfv8xpi8rnhlin5jm8f58ltkq-1}
    #{x pfv8xpi8rnhlin5jm8f58ltkq-1}))
> ((lambda (x) (* x x)) (+ 3 4))
49
> (lambda (x) (* x x))
#<procedure>
> (expand '(lambda (x) (* x x)))
(lambda (#{x pfv8xpi8rnhlin5jm8f58ltkq-2})
  (#2%*
    #{x pfv8xpi8rnhlin5jm8f58ltkq-2}
    #{x pfv8xpi8rnhlin5jm8f58ltkq-2}))
> ((lambda (x) (* x x)) (+ 3 4))
49
> ((lambda (x) (* x x)) ; => procedure     x = 7
   (+ 3 4) ; => 7
   )
49
> (define f (lambda (x) (* x x)))
> f
#<procedure f>
> (f (+ 3 4))
49
> f
#<procedure f>
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e)) body) ; pattern
       ((lambda (x) body) e) ; template
       )))
> (my-let ((y (+ 3 4)))
    (* y y))
;; =>
((lambda (y) (* y y)) (+ 3 4))
49
> (+ 3 4)
7
> (lambda (x) x)
#<procedure>
> (let ((x (+ 3 4)))
    (* x x))
49
> let

Exception: invalid syntax let
Type (debug) to enter the debugger.
> (let ()
    (+ 3 4))
7
> (let ((y (+ 3 4))
        (z 5)
        (w (* 3 4)))
    (list y w z))
(7 12 5)
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e)) body) ; pattern
       ((lambda (x) body) e) ; template
       )))
> (my-let ((y (+ 3 4))
           (z 5)
           (w (* 3 4)))
    (list y w z))

Exception: invalid syntax (my-let ((y (...)) (z 5) (w (...))) (list y w z))
Type (debug) to enter the debugger.
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e) ...) body) ; pattern
       ((lambda (x ...) body) e ...) ; template
       )))
> (my-let ((y (+ 3 4))
           (z 5)
           (w (* 3 4)))
    (list y w z))
(7 12 5)
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e) ...) body) ; pattern
       ((lambda (x) body) e ...) ; template
       )))

Exception: missing ellipsis in syntax form (syntax ((lambda (...) body) e ...))
Type (debug) to enter the debugger.
> (define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e) ...) body) ; pattern
       ((lambda (x ...) body) e ...) ; template
       )))
> (my-let ((y (+ 3 4))
        (z 5)
        (w (* 3 4)))
    (list y w z))
(7 12 5)
> (my-let ()
    (+ 3 4))
7
> (my-let ((y (+ 3 4))
           (z 5)
           (w (* 3 4)))
    (list y w z))
(7 12 5)
> ((lambda (y z w) (list y w z))
   (+ 3 4)
   5
   (* 3 4))
(7 12 5)
> let*

Exception: invalid syntax let*
Type (debug) to enter the debugger.
> letfoo

Exception: variable letfoo is not bound
Type (debug) to enter the debugger.
> +
#<procedure +>
> cons
#<procedure cons>
> (let ((x (+ 3 4))
        (y 7))
    (list x y))
(7 7)
> (let ((x (+ 3 4))
        (y x))
    (list x y))

Exception: variable x is not bound
Type (debug) to enter the debugger.
> (let ((x (+ 3 4))
        (y x))
    (list x y))

Exception: variable x is not bound
Type (debug) to enter the debugger.
> ((lambda (x y) (list x y))
   (+ 3 4)
   x)

Exception: variable x is not bound
Type (debug) to enter the debugger.
> ((lambda (x y)
     (list x y))
   (+ 3 4)
   x)

Exception: variable x is not bound
Type (debug) to enter the debugger.
> (let ((x 5))
    (let ((x (+ 3 4))
          (y x))
      (list x y)))
(7 5)
> (let ((x 5))
    (let ((x (+ 3 4)g)
          (y x))
      (list x y)))

Exception: invalid syntax (let ((x (...) g) (y x)) (list x y))
Type (debug) to enter the debugger.
> (let ((x 5))
    (let ((x (+ 3 4))
          (y x))
      (list x y)))
(7 5)
> (let ((x 5))
    (let ((y x)
          (x (+ 3 4)))
      (list x y)))
(7 5)
> ((lambda (x y) (list x y))
   (+ 3 4)
   5)
(7 5)
> ((lambda (y x) (list x y))
   5
   (+ 3 4))
(7 5)
> (let ((x 5))
    (let ((x (+ 3 4))
          (y x))
      (list x y)))
(7 5)
> (let ((x (+ 3 4))
        (y x))
    (list x y))

Exception: variable x is not bound
Type (debug) to enter the debugger.
> (let ((x (+ 3 4))
    (let ((y x))
      (list x y))))

Exception: invalid syntax (let ((x (...)) (let (...) (...))))
Type (debug) to enter the debugger.
> (let ((x (+ 3 4)))
    (let ((y x))
      (list x y)))
(7 7)
> (let* ((x (+ 3 4))
         (y x))
    (list x y))
(7 7)
> (expand '(let* ((x (+ 3 4))
                  (y x))
             (list x y)))
(let ([#{x pfv8xpi8rnhlin5jm8f58ltkq-3} (#2%+ 3 4)])
  (let ([#{y pfv8xpi8rnhlin5jm8f58ltkq-4} #{x pfv8xpi8rnhlin5jm8f58ltkq-3}])
    (#2%list
      #{x pfv8xpi8rnhlin5jm8f58ltkq-3}
      #{y pfv8xpi8rnhlin5jm8f58ltkq-4})))
> (print-gensym #f)
> (expand '(let* ((x (+ 3 4))
                  (y x))
             (list x y)))
(let ([x (#2%+ 3 4)])
  (let ([y x])
    (#2%list x y)))
> (expand '(let* ((x (+ 3 4))
                  (y x)
                  (x y))
             (list x y)))
(let ([x (#2%+ 3 4)])
  (let ([y x])
    (let ([x y])
      (#2%list x y))))
> (define-syntax my-let*
    (syntax-rules ()
      (())
      ))

Exception: invalid syntax-rules clause (())
Type (debug) to enter the debugger.
> (let* ()
    (+ 3 4))
7
> (let ()
    (+ 3 4))
7
> (expand '(let* ()
             (+ 3 4)))
(#2%+ 3 4)
> (define-syntax my-let*
    (syntax-rules ()
      ((my-let* () body)
       body)))
> (my-let* ()
    (+ 3 4))
7
> (define-syntax my-let*
    (syntax-rules ()
      ((my-let* () body)
       body)
      ((my-let* ((x e) (x* e*) ...) body)
       (let ((x e))
         (my-let* ((x* e*) ...)
           body)))))
> (let* ((x (+ 3 4))
         (y x))
    (list x y))
(7 7)
> (my-let* ((x (+ 3 4))
            (y x))
           (list x y))
(7 7)
> (my-let* ()
    (+ 3 4))
7
> ((lambda (x)
     y)
   (+ 2 3))

Exception: variable y is not bound
Type (debug) to enter the debugger.
> ((lambda (x)
     x)
   (+ 2 3))
5
> ((lambda (x y)
     (+ x y))
   (+ 2 3)
   6)
11
> (lambda (x)
    (lambda (y)
      (lambda (x)
        (+ x y))))
#<procedure>
> ((lambda (x)
    (lambda (y)
      (lambda (x)
        (+ x y))))
   1)
#<procedure>
> (((lambda (x) ; 1
      (lambda (y) ; 2
        (lambda (x)
          (+ x y))))
    1)
   2)
#<procedure>
> ((((lambda (x) ; 1
       (lambda (y) ; 2
         (lambda (x) ; 3
           (+ x y))))
     1)
    2)
   3)
5
> ((((lambda (x) ; 1
       (lambda (y) ; 2
         (lambda (x) ; 3
           (+ x y))))
     1)
    2)
   3)
5
> (let ((x 1))
    (let ((y 2))
      (let ((x 3))
        (+ x y))))
5
> (let ((foo 1))
    (let ((y 2))
      (let ((x 3))
        (+ x y))))
5
> (let* ((x 1)
         (y 2)
         (x 3))
    (+ x y))
5
> (my-let* ((x 1)
         (y 2)
         (x 3))
    (+ x y))
5
> (let* ((x 1)  ; variable scope
         (y 2)  ; lexical scope   static scope
         (x 3))
    (+ x y))
5
> ((lambda (x)
     (let ((y x))
       (let* ((w y)
              (v w))
         (list x y w v))))
   5)
(5 5 5 5)
> ((lambda (x)
     (let ((y x)
           (z y))
       (let* ((w y)
              (v w))
         (list x y w v))))
   5)

Exception: variable y is not bound
Type (debug) to enter the debugger.
> ((lambda (x) ; ((x . 5)) <- environment
     (let ((y x)) ; ((y . 5) (x . 5))
       (let* ((w y)
              (v w))
         (list x y w v))))
   5)
(5 5 5 5)
> 
;; ()   empty environment
(let ((x (+ 2 3)))
  ;; ((x . 5))
  (let ((y (* x x)))
    ;; ((y . 25) (x . 5))
    (let ((z (+ x y)))
      ;; ((z . 30) (y . 25) (x . 5))
      (list x y z))))
(5 25 30)
> 
;; ()   empty environment
(let ((x (+ 2 3)))
  ;; ((x . 5))
  (let ((y (* x x)))
    ;; ((y . 25) (x . 5))
    (let ((z (+ x y)))
      ;; ((z . 30) (y . 25) (x . 5))
      (let ((x z))
        ;; ((x . 30) (z . 30) (y . 25) (x . 5))
        (list x y z)))))
(30 25 30)
> (let ((x (+ 2 3)))
    (let ((x 7))
      x))
7
> (let ((x (+ 2 3)))
    ;; ((x . 5))
    (list
      (let ((x 7))
        ;; ((x . 7) (x . 5))
        x)
      x))
(7 5)
> (let ((x (+ 2 3)))
    ;; ((x . 5))
    (list
      (let ((y 7))  ;; alpha-renaming
        ;; ((y . 7) (x . 5))
        y)
      x))
(7 5)
> 