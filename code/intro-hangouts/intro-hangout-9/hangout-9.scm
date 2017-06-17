(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")


(define rembero
  (lambda (x ls ls-x)
    (conde
      [(== '() ls) (== '() ls-x)]
      [(fresh (d)
         (== `(,x . ,d) ls)
         (== d ls-x))]
      [(fresh (a d res)
         (== `(,a . ,d) ls)
         (=/= x a)
         (== (cons a res) ls-x)
         (rembero x d res))])))

(define rembero
  (lambda (x ls ls-x)
    (conde
      [(== '() ls) (== '() ls-x)]
      [(fresh (a d)
         (== `(,a . ,d) ls)
         (== x a)
         (== d ls-x))]
      [(fresh (a d res)
         (== `(,a . ,d) ls)
         (=/= x a)
         (== (cons a res) ls-x)
         (rembero x d res))])))

(define rembero
  (lambda (x ls ls-x)
    (conde
      [(== '() ls) (== '() ls-x)]
      [(fresh (a d)
         (== `(,a . ,d) ls)
         (conde
           [(== x a) (== d ls-x)]
           [(=/= x a)
            (fresh (res) 
              (== (cons a res) ls-x)
              (rembero x d res))]))])))


;; x is a symbol        [var]
;; (lambda (,x) ,e)  [lambda/abstraction]
;; (,e1 ,e2)            [application]

(define parseo
  (lambda (expr)
    (conde
      [(numbero expr)]
      [(symbolo expr)]
      [(fresh (x e)
         (== `(lambda (,x) ,e) expr)
         (symbolo x)
         (parseo e))]
      [(fresh (x e body)
         (== `(let ((,x ,e)) ,body) expr)
         (symbolo x)
         (parseo e)
         (parseo body))]
      [(fresh (e1 e2)
         (== `(,e1 ,e2) expr)
         (parseo e1)
         (parseo e2))])))

(define parseo
  (lambda (expr legal)
    (conde
      [(numbero expr) (== #f legal)]
      [(symbolo expr) (== #t legal)]
      [(fresh (e l)
         (== `(lambda () ,e) expr)
         (== #f legal)
         (parseo e l))]
      [(fresh (x y z* e l)
         (== `(lambda (,x ,y . ,z*) ,e) expr)
         (== #f legal)
         (parseo e l))]
      [(fresh (x e)
         (== `(lambda (,x) ,e) expr)
         (symbolo x)
         (conde
           [(== #t legal) (parseo e #t)]
           [(== #f legal) (parseo e #f)]))]
      [(fresh (e1 e2 l)
         (== `(,e1 ,e2) expr)
         (conde
           [(== #t legal)
            (parseo e1 #t)
            (parseo e2 #t)]
           [(== #f legal)
            (parseo e1 #f)
            (parseo e2 l)]
           [(== #f legal)
            (parseo e1 #t)
            (parseo e2 #f)]))])))








#!eof


;; (member? 'x '(y x z)) => #t
;; (member? 'x '(y w z)) => #f

;; (rember 'x '()) => ()
;; (rember 'x '(y x z)) => (y z)
;; (rember 'x '(x y z)) => (y z)
;; (rember 'x '(y x z x)) => (y z x)
;; (rember 'x '(x y z x)) => (y z x)
;; (rember 'v '(y x z x)) => (y x z x)

;; simple, original rember
(define rember
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? (car ls) x) (cdr ls)]
      [else (cons (car ls) (rember x (cdr ls)))])))

(define not-eqv?
  (lambda (v1 v2)
    (not (eqv? v1 v2))))

;; add Dijkstra guards
(define rember
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(and (pair? ls)
            (eqv? (car ls) x))
       (cdr ls)]
      [(and (pair? ls)
            (not-eqv? (car ls) x))
       (cons (car ls) (rember x (cdr ls)))])))

;; unnesting
(define rember
  (lambda (x ls)
    (cond
      [(let ((pt (pair? ls)))
         (and pt
              (let ((a (car ls)))
                (let ((net (not-eqv? a x)))
                  net))))
       (let ((d (cdr ls)))
         (let ((a (car ls)))
           (let ((res (rember x d)))
             (cons a res))))]
      [(let ((pt (pair? ls)))
         (and pt
              (let ((a (car ls)))
                (let ((et (eqv? a x)))
                  et))))
       (cdr ls)]
      [(null? ls) '()])))

;; exercises:
;; 1. define 'multi-rember': removes *all* occurrences of 'x' from 'ls'
;; 2. define 'multi-rembero' in miniKanren
;; 3. define 'rember*': removes *all* 'x's from a deeply-nested list
;; 4. define 'rember*o'
