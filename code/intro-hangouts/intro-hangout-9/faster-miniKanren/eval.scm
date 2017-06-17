(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")

(define lookupo
  (lambda (x env val)
    (fresh (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (symbolo y)     
      (conde
        [(== x y) (== val v)]
        [(=/= x y)
         (lookupo x env^ val)]))))

(define evalo
  (lambda (expr env value)
    (conde
      [(fresh (c)
         (== `(const ,c) expr)         
         (== value c)
         (absento 'closure c))]
      [(symbolo expr) ; variable
       (lookupo expr env value)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) value))]
      [(fresh (e1 e2 x body env^ arg)
         (== `(apply ,e1 ,e2) expr)
         (evalo e1 env `(closure ,x ,body ,env^))
         (evalo e2 env arg)
         (evalo body `((,x . ,arg) . ,env^) value))]
      [(fresh (e1 e2 n1 n2)
         (== `(+ ,e1 ,e2) expr)
         (evalo e1 env n1)
         (evalo e2 env n2)
         (pluso n1 n2 value))]
      [(fresh (e1 e2 n1 n2)
         (== `(* ,e1 ,e2) expr)
         (evalo e1 env n1)
         (evalo e2 env n2)
         (*o n1 n2 value))])))

(run* (q)
  (evalo `(+ (const ,(build-num 1))
             (const ,(build-num 2)))
         '()
         q))

(run 10 (expr)
  (evalo expr
         '()
         (build-num 3)))
