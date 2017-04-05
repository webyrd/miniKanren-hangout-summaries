(load "pmatch.scm")

(pmatch 5
  ((,one ,two)
   one)
  ((,one ,two ,three)
   (list three two one))
  ((,first . ,rest)
   rest)
  (else 'no-match))

(define my-match
  (lambda (x)
    (pmatch x
      ((,one ,two)
       one)
      ((,one ,two ,three)
       (list three two one))
      ((,first . ,rest)
       rest)
      (else 'no-match))))

(define valid-expression?
  (lambda (expr)
    (pmatch expr
      (,x (guard (symbol? x))
       #t)
      (,b (guard (boolean? b))
       #t)
      (,n (guard (number? n))
       #t)
      ((lambda (,x) ,body)
       #t)
      ((,e1 ,e2) ;; procedure application
       #t)
      (else
       #f))))


;; lambda calculus

;; x               <- variable
;; (lambda (x) e)  <- lambda or abstraction
;; (e e)           <- application

(define lambda-calculus-expression?
  (lambda (expr)
    (pmatch expr
      (,x  ;; variable
       (guard (symbol? x))
       #t)
      ((lambda (,x) ,body) ;; lambda/abstraction
       (guard (symbol? x))
       (lambda-calculus-expression? body))
      ((,e1 ,e2) ;; application
       (and
         (lambda-calculus-expression? e1)
         (lambda-calculus-expression? e2)))
      (else
       #f))))



;; Church-encoding of arithmetic













#!eof

;; member
;; rember, rember*, deep-rember*
;; filter
;; map

(member 'x '()) => #f
(member 'x '(a x f x g)) => #t
(member 'x '(x f x g)) => #t
(member 'x '(a f g)) => #f
(member 'x '(x f g)) => #t
(member 'foo '(a foo g)) => #t

(define member
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((equal? (car l) x) #t)
      (else (member x (cdr l))))))

(rember 'x '()) => ()
(rember 'x '(x f x g)) => (f x g)
(rember 'x '(f g)) => (f g)
(rember 'x '(f x g)) => (f g)

(define rember
  (lambda (x l)
    (cond
      ((null? l) '())
      ((equal? (car l) x) (cdr l))
      (else (cons (car l) (rember x (cdr l)))))))

(rember* 'x '()) => ()
(rember* 'x '(x f x g)) => (f g)
(rember* 'x '(f g)) => (f g)
(rember* 'x '(f x g)) => (f g)
(rember* 'x '((x x) g x (f (x a)) (x (((h x)) j)) x b)) =>
((x x) g (f (x a)) (x (((h x)) j)) b)

(define rember*
  (lambda (x l)
    (cond
      ((null? l) '())
      ((equal? (car l) x) (rember* x (cdr l)))
      (else (cons (car l) (rember* x (cdr l)))))))

(deep-rember* 'x '()) => ()
(deep-rember* 'x '((x x) g x (f (x a)) (x (((h x)) j)) x b))
=>
(() g (f (a)) ((((h)) j)) b)

(define deep-rember*
  (lambda (x l)
    (cond
      ((null? l) '())
      ;; new clause for deep recursion
      ((pair? (car l))
       (cons (deep-rember* x (car l))
             (deep-rember* x (cdr l))))
      ((equal? (car l) x) (deep-rember* x (cdr l)))
      (else (cons (car l) (deep-rember* x (cdr l)))))))

;;; filter-in
(filter odd? '()) => '()
(filter odd? '(1 2 3 4 5 6)) => (1 3 5)
(filter odd? '(2 3 4 5 6)) => (3 5)

(define filter
  (lambda (f l)
    (cond
      ((null? l) '())
      ((f (car l))
       (cons (car l) (filter f (cdr l))))
      (else (filter f (cdr l))))))



;;; quotation

(+ 3 4)

'(+ 3 4) is equivalent to (quote (+ 3 4))

(quote datum) => datum

-----------------------

;; quasiquote, backquote, backtick

`(a b c) is equivalent to (quasiquote (a b c))

;; unquote, comma

,x is equivalent to (unquote x)

(define x 5)

(3 x 17) => (3 5 17)

(list 3 x 17)
(cons 3 (cons x (cons 17 '())))

(define x 6)

`(3 ,x 17) = (quasiquote (3 (unquote x) 17))
 (3 6 17)

(let ((x 7))
  x)
=
((lambda (x) x) 7)

(let ((x 7))
  `(3 ,x 17))



(define make-my-list
  (lambda (x)
    `(3 ,x 17)))

(define make-my-list-flatten
  (lambda (x)
    `(3 ,@x 17)))

;; unquote-splicing

`(a ,(+ 3 4) c)

--------------------------------

;; macros and syntactic abstraction

(let ((x (+ 3 4)))
  (* x x))

is equivalent to

((lambda (x) (* x x))
 (+ 3 4))

=>
49

my-let


;;        x    e
(my-let ((x (+ 3 4)))
  ;; body
  (* x x))

(my-let ((x (+ 3 4)))
  (* x x))


expand =>

((lambda (x) (* x x))
 (+ 3 4))


(define-syntax my-let
  (syntax-rules ()
    ((my-let ((x e)) body) ;; pattern
     ;; template
     ((lambda (x) body)
      e))))


