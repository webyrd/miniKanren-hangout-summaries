(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")






;; Scheme/functional world
;; car & cdr & returning values
(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(equal? (car l) x) #t]
      [else (member? x (cdr l))])))

(define membero
  (lambda (x l)
    (conde
      [(fresh (a d)
         (== (cons a d) l)
         (== a x))]
      [(fresh (a d)
         (== (cons a d) l)
         (=/= a x)
         (membero x d))])))

(define membero
  (lambda (x l)
    (fresh (a d)
      (== (cons a d) l)
      (conde
        [(== a x)]
        [(=/= a x) (membero x d)]))))

(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(and (not (null? l))
            (equal? (car l) x))
       #t]
      [(and (not (null? l))
            (not (equal? (car l) x)))
       (member? x (cdr l))])))



;; miniKanren/relational world
;; == & cons & associating values with 'out'
(define membero
  (lambda (x l x-in-l)
    (conde
      [(== '() l) (== #f x-in-l)]
      [(fresh (a d) ; first thing in l is a
         (== (cons a d) l)
         (== a x)
         (== #t x-in-l))]
      [(fresh (a d) ; first thing in l isn't a
         (== (cons a d) l)
         (=/= a x) ;; disequality constraint
         (membero x d x-in-l))])))

(define membero
  (lambda (x l x-in-l)
    (conde
      [(fresh (a d) ; first thing in l isn't a
         (== (cons a d) l)
         (=/= a x) ;; disequality constraint
         (membero x d x-in-l))]
      [(fresh (a d) ; first thing in l is a
         (== (cons a d) l)
         (== a x)
         (== #t x-in-l))]
      [(== '() l) (== #f x-in-l)])))




#!eof

car   caro
cdr   cdro
cons  conso

;; l = (1 2 3 4)
(lambda (l)
  (fresh (a d)
    (== (cons a d) l)))

(== `(,a . ,d)      ;; a = 1
    `(1 . (2 3 4))) ;; d = (2 3 4)

(car '(1 2 3 4 5)) => 1
(caro '(1 2 3 4 5) out) => out = 1
(caro '(1 2 3 4 5) 1) => succeed
(caro '(1 2 3 4 5) 2) => fail
(caro l 2) => l = (2 . d)

(define caro
  (lambda (l out)
    (fresh (a d)
      (== (cons a d) l)
      (== a out))))

(define caro
  (lambda (l a)
    (fresh (_)
      (== (cons a _) l))))

(define cdro
  (lambda (l d)
    (fresh (_)
      (== (cons _ d) l))))

(cons 5 '(6 7)) => (5 6 7)

(define conso
  (lambda (a d out)
    (== (cons a d) out)))
