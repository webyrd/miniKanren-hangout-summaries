;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define (evalo expr val)
  ((make-eval-expo 'top-level) expr initial-env val))

(define (make-eval-expo context)
  (case context
    ((top-level)
     (eval-expo-with-default-ordering context))
    ((lambda-multi lambda-variadic)
     (eval-expo-with-default-ordering context))
    ((null?)
     (eval-expo-with-default-ordering context))
    ((if-test)
     (eval-expo-with-default-ordering context))
    ((if-alt)
     (eval-expo-with-default-ordering context))
    ((cons-a)
     (eval-expo-with-default-ordering context))
    ((cons-d)
     (eval-expo-with-default-ordering context))
    ((equal?)
     (eval-expo-with-default-ordering context))
    (else
     (eval-expo-with-default-ordering context))))

(define (eval-expo-with-default-ordering context)
  (lambda (expr env val)
    (conde
      ((== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env))

      ((numbero expr) (== expr val))

      ((symbolo expr) (lookupo expr env val))

      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (== `(closure (lambda ,x ,body) ,env) val)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))
    
      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) ;; FIX ME??  should this really be app-rator?  or something like prim-rator?
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'app-rand* ;; FIX ME??  should this really be app-rand*?  or something like prim-rand*?
                     )))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val))
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . symbol?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((symbolo v) (== #t val))
             ((numbero v) (== #f val))
             ((fresh (a d)
                (== `(,a . ,d) v)
                (== #f val)))))
         (eval-listo rands env a* 'app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'app-rand*)))
    
      ((handle-matcho expr env val context))

      ((fresh (p-name x body letrec-body)
         ;; single-function variadic letrec version
         (== `(letrec ((,p-name (lambda ,x ,body)))
                ,letrec-body)
             expr)
         (conde
                                        ; Variadic
           ((symbolo x))
                                        ; Multiple argument
           ((list-of-symbolso x)))
         (not-in-envo 'letrec env)
         ((make-eval-expo 'letrec-body)
          letrec-body
          `((,p-name . (rec . (lambda ,x ,body))) . ,env)
          val)))

      ((boolean-primo expr env val context))
      ((and-primo expr env val context))
      ((or-primo expr env val context))
      ((if-primo expr env val context))
    
      )))

(define empty-env '())

(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (eval-listo expr env val context)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       ((make-eval-expo context) a env v-a)
       (eval-listo d env v-d context)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

#|
(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))
|#

(define (boolean-primo expr env val context ; can ignore context in this case
                       )
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (and-primo expr env val context)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val context)))

(define (ando e* env val context)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       ((make-eval-expo 'and) e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          ((make-eval-expo 'and) e1 env v))
         ((=/= #f v)
          ((make-eval-expo 'and) e1 env v)
          (ando `(,e2 . ,e-rest) env val context)))))))

(define (or-primo expr env val context)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val context)))

(define (oro e* env val context)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       ((make-eval-expo 'or) e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          ((make-eval-expo 'or) e1 env v))
         ((== #f v)
          ((make-eval-expo 'or) e1 env v)
          (oro `(,e2 . ,e-rest) env val context)))))))

(define (if-primo expr env val context)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    ((make-eval-expo 'if-test) e1 env t)
    (conde
      ((=/= #f t) ((make-eval-expo 'if-conseq) e2 env val))
      ((== #f t) ((make-eval-expo 'if-alt) e3 env val)))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      . ,empty-env))

(define handle-matcho
  (lambda  (expr env val context)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      ((make-eval-expo 'match-against) against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         ((make-eval-expo 'match-result) result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))
