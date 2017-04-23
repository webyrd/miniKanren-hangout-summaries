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

(define eval-expr-lexical
  (lambda (expr env)
    (pmatch expr
      [,n (guard (number? n))
       n]
      [(zero? ,e)
       (zero? (eval-expr-lexical e env))]      
      [(add1 ,e)
       (add1 (eval-expr-lexical e env))]
      [(sub1 ,e)
       (sub1 (eval-expr-lexical e env))]
      [(* ,e1 ,e2)
       (* (eval-expr-lexical e1 env) (eval-expr-lexical e2 env))]
      [(+ ,e1 ,e2)
       (+ (eval-expr-lexical e1 env) (eval-expr-lexical e2 env))]     
      [(if ,e1 ,e2 ,e3)
       (if (eval-expr-lexical e1 env)
           (eval-expr-lexical e2 env)
           (eval-expr-lexical e3 env))]
      #|
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (eval-expr-lexical `((lambda (,x) ,body) ,e) env)]
      |#
      
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (let ((arg (eval-expr-lexical e env)))
         (eval-expr-lexical body (lambda (y)
                                   (if (eq? x y)
                                       arg
                                       (env y)))))]
      
      [,x (guard (symbol? x))
       (env x)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (lambda (arg env^)
         (eval-expr-lexical body (lambda (y)
                                   (if (eq? x y)
                                       arg
                                       (env y)))))]
      [(,rator ,rand)
       ((eval-expr-lexical rator env) (eval-expr-lexical rand env) env)])))


(define my-eval-lexical
  (lambda (expr)
    (eval-expr-lexical expr
                       (lambda (y) (error 'lookup (format "unbound variable ~s" y))))))

(my-eval-lexical '(lambda (z) z))

(test "lexical let 1"
  (my-eval-lexical '(let ((z (* 3 4))) (sub1 z)))
  11)

(test "lexical let 2"
  (my-eval-lexical '(let ((x (sub1 6)))
                      (let ((f (lambda (y) (+ y x))))
                        (let ((x (* 3 4)))
                          (f x)))))
  17)

(test "lexical let 3"
  (my-eval-lexical '(let ((x (sub1 6)))
                      (let ((f (lambda (y) (+ y x))))
                        (let ((y (* 3 4)))
                          (f y)))))
  17)

(test "lexical ! 5"
  (my-eval-lexical '(((lambda (!)
                        (lambda (n)
                          ((! !) n)))
                      (lambda (!)
                        (lambda (n)
                          (if (zero? n)
                              1
                              (* n ((! !) (sub1 n)))))))
                     5))
  120)


;;;;;


(define eval-expr-dynamic
  (lambda (expr env)
    (pmatch expr
      [,n (guard (number? n))
       n]
      [(zero? ,e)
       (zero? (eval-expr-dynamic e env))]      
      [(add1 ,e)
       (add1 (eval-expr-dynamic e env))]
      [(sub1 ,e)
       (sub1 (eval-expr-dynamic e env))]
      [(* ,e1 ,e2)
       (* (eval-expr-dynamic e1 env) (eval-expr-dynamic e2 env))]
      [(+ ,e1 ,e2)
       (+ (eval-expr-dynamic e1 env) (eval-expr-dynamic e2 env))]     
      [(if ,e1 ,e2 ,e3)
       (if (eval-expr-dynamic e1 env)
           (eval-expr-dynamic e2 env)
           (eval-expr-dynamic e3 env))]
      #|
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (eval-expr-dynamic `((lambda (,x) ,body) ,e) env)]
      |#
      
      [(let ((,x ,e)) ,body) (guard (symbol? x))
       (let ((arg (eval-expr-dynamic e env)))
         (eval-expr-dynamic body (lambda (y)
                                   (if (eq? x y)
                                       arg
                                       (env y)))))]
      
      [,x (guard (symbol? x))
       (env x)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (lambda (arg env^)
         (eval-expr-dynamic body (lambda (y)
                                   (if (eq? x y)
                                       arg
                                       (env^ y)))))]
      [(,rator ,rand)
       ((eval-expr-dynamic rator env) (eval-expr-dynamic rand env) env)])))


(define my-eval-dynamic
  (lambda (expr)
    (eval-expr-dynamic expr
                       (lambda (y) (error 'lookup (format "unbound variable ~s" y)))1)))

(my-eval-dynamic '(lambda (z) z))

(test "dynamic let 1"
  (my-eval-dynamic '(let ((z (* 3 4))) (sub1 z)))
  11)

(test "dynamic let 2"
  (my-eval-dynamic '(let ((x (sub1 6)))
                      (let ((f (lambda (y) (+ y x))))
                        (let ((x (* 3 4)))
                          (f x)))))
  24)

(test "dynamic let 3"
  (my-eval-dynamic '(let ((x (sub1 6)))
                      (let ((f (lambda (y) (+ y x))))
                        (let ((y (* 3 4)))
                          (f y)))))
  17)

(test "dynamic ! 5"
  (my-eval-dynamic '(((lambda (!)
                        (lambda (n)
                          ((! !) n)))
                      (lambda (!)
                        (lambda (n)
                          (if (zero? n)
                              1
                              (* n ((! !) (sub1 n)))))))
                     5))
  120)
