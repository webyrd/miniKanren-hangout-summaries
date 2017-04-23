(load "pmatch.scm")

(define-syntax test
  (syntax-rules ()
    [(test name expr expected-val)
     (let ((v expr))
       (if (equal? v expected-val)
           (begin
             (display "passed test ")
             (write name)
             (newline))
           (error 'name
             (format "\nTest ~s failed!!\nExpected ~s, but got ~s"
                     name
                     expected-val
                     v))))]))

;; representation dependent with respect to environments and procedures

(define eval-expr
  (lambda (expr env)
    (pmatch expr
      [,n (guard (number? n))
       n]
      [(zero? ,e)
       (zero? (eval-expr e env))]      
      [(add1 ,e)
       (add1 (eval-expr e env))]
      [(sub1 ,e)
       (sub1 (eval-expr e env))]
      [(* ,e1 ,e2)
       (* (eval-expr e1 env) (eval-expr e2 env))]
      [(+ ,e1 ,e2)
       (+ (eval-expr e1 env) (eval-expr e2 env))]     
      [(if ,e1 ,e2 ,e3)
       (if (eval-expr e1 env)
           (eval-expr e2 env)
           (eval-expr e3 env))]
      #|
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (eval-expr `((lambda (,x) ,body) ,e) env)]
      |#
      
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (let ((arg (eval-expr e env)))
         (eval-expr body (ext-env x arg env)))]
      
      [,x (guard (symbol? x))
       (apply-env x env)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (lambda (arg)
         (eval-expr body (ext-env x arg env)))]
      [(,rator ,rand)
       ((eval-expr rator env) (eval-expr rand env))])))


(define my-eval
  (lambda (expr)
    (eval-expr expr (make-empty-env))))

;;; environment helpers -- higher order (procedural/functional representation)
#|
(define make-empty-env
  (lambda ()
    (lambda (y) (error 'lookup (format "unbound variable ~s" y)))))

(define apply-env
  (lambda (x env)
    (env x)))

(define ext-env
  (lambda (x arg env)
    (lambda (y)
      (if (eq? x y)
          arg
          (apply-env y env)))))
|#


;;; environment helpers -- first order (data structural/a-list representation)
#|
(define make-empty-env
  (lambda ()
    '()))

(define apply-env
  (lambda (x env)
    (cond
      ((null? env)
       (error 'lookup (format "unbound variable ~s" x)))
      ((eq? (caar env) x)
       (cdar env))
      (else (apply-env x (cdr env))))))

(define ext-env
  (lambda (x arg env)
    `((,x . ,arg) . ,env)))
|#

;;; environment helpers -- first order (data structural/tagged list representation)
;; exampples:
;; (empty-env)
;; (ext-env w (a b c) (ext-env z 5 (empty-env)))

(define make-empty-env
  (lambda ()
    '(empty-env)))

(define apply-env
  (lambda (y env^)
    (pmatch env^
      [(empty-env)
       (error 'lookup (format "unbound variable ~s" y))]
      [(ext-env ,x ,arg ,env)
       (if (eq? x y)
           arg
           (apply-env y env))])))

(define ext-env
  (lambda (x arg env)
    `(ext-env ,x ,arg ,env)))


(my-eval '(lambda (z) z))

(test "let 1"
  (my-eval '(let ((z (* 3 4))) (sub1 z)))
  11)

(test "let 2"
  (my-eval '(let ((x (sub1 6)))
              (let ((f (lambda (y) (+ y x))))
                (let ((x (* 3 4)))
                  (f x)))))
  17)

(test "let 3"
  (my-eval '(let ((x (sub1 6)))
              (let ((f (lambda (y) (+ y x))))
                (let ((y (* 3 4)))
                  (f y)))))
  17)

(test "! 5"
  (my-eval '(((lambda (!)
                (lambda (n)
                  ((! !) n)))
              (lambda (!)
                (lambda (n)
                  (if (zero? n)
                      1
                      (* n ((! !) (sub1 n)))))))
             5))
  120)
