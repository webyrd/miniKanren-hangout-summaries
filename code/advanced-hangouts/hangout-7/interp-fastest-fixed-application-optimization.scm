;; Use Greg's optimization for variable-argument application.

;; be a little less dumb
;;
;; * variadic lambdas should come after multi-argument lambdas

;; FIXME -- symbol?, and perhaps other type predicates, doesn't handle booleans (fails)
;;
;; Compare:
;; > (run* (q)
;;     (evalo
;;      `(letrec ((map
;;                 (lambda (p ls)
;;                   (if (null? ls)
;;                       '()
;;                       (cons (p (car ls)) (map p (cdr ls)))))))
;;         (map symbol? '(5 6 foo 7)))
;;      q))
;; =>
;; ((#f #f #t #f))

;; > (run* (q)
;;     (evalo
;;      `(letrec ((map
;;                 (lambda (p ls)
;;                   (if (null? ls)
;;                       '()
;;                       (cons (p (car ls)) (map p (cdr ls)))))))
;;         (map symbol? '(5 6 foo 7 #t)))
;;      q))
;; =>
;; ()


;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define-syntax let/vars
  (syntax-rules ()
    ((_ _ () body) body)
    ((_ _ () body ...) (begin body ...))
    ((_ st (qvar ...) body ...)
     (let ((scope (subst-scope (state-S st))))
       (let ((qvar (var scope)) ...)
         body ...)))))

(define (list-split-ground st xs)
  (let loop ((rprefix '()) (xs xs))
    (let ((tm (walk xs (state-S st))))
      (if (pair? tm)
        (loop (cons (walk (car tm) (state-S st)) rprefix) (cdr tm))
        (values rprefix xs)))))

(define (eval-application rands aenv a* body-goal)
  (define succeed unit)
  (lambdag@ (st)
    (let-values (((rrands rands-suffix) (list-split-ground st rands)))
      (let-values
        (((ggoals vgoals args-suffix)
          (let loop ((rands (reverse rrands))
                     (ggoals succeed)
                     (vgoals succeed)
                     (args a*))
            (if (null? rands) (values ggoals vgoals args)
              (let ((rand (car rands)))
                (let/vars st (args-rest)
                  (let ((goal (fresh (arg)
                                (== `(,arg . ,args-rest) args)
                                ((make-eval-expo 'app-rand*) rand aenv arg))))
                    (if (var? rand)
                      (loop (cdr rands) ggoals (fresh () vgoals goal) args-rest)
                      (loop (cdr rands) (fresh () ggoals goal) vgoals args-rest)))))))))
        ((fresh ()
           ggoals    ; try ground arguments first
           body-goal ; then the body
           vgoals    ; then fill in unbound arguments
           ; any unbound final segment of arguments
           (eval-listo rands-suffix aenv args-suffix 'app-rand*)) st)))))



(define (evalo expr val)
  ((make-eval-expo 'top-level) expr initial-env val))

(define (make-eval-expo context)
  (case context
    ((top-level)
     (eval-expo-with-default-ordering context))

    ;; These two cases seem to not help, and indeed very slightly slow
    ;; down synthesis.  I suspect this is because of work/unifications
    ;; and introductions of logic variables.  I suspect much duplicate
    ;; work introduced in the "inlining" transformation can be
    ;; removed.
    ;;
    ((app-rator)
     (eval-expo-app-rator-ordering context))
    ((prim-app-rator)
      (eval-expo-prim-app-rator-ordering context))

    ;; does something!
    ((app-rand*)
     (eval-expo-app-rand*-ordering context))
    ;; end does something

    ((lambda-multi lambda-variadic)
     ;; consolidating both types of lambda for now
     (eval-expo-lambda-ordering context))
    ((null?)
     (eval-expo-with-default-ordering context))
    ;; does something!
    ((if-test)
     (eval-expo-if-test-ordering context))
    ((if-conseq)
     (eval-expo-if-conseq-ordering context))
    ((if-alt)
     (eval-expo-if-alt-ordering context))
    ;; end does something
    ((cons-a)
     (eval-expo-with-default-ordering context))
    ((cons-d)
     (eval-expo-with-default-ordering context))
    ((equal?)
     (eval-expo-with-default-ordering context))
    (else
     (eval-expo-with-default-ordering context))))

(define-syntax eval-expo-specialization
  (syntax-rules ()
    [(_ clause* ...)
     (lambda (context)
       (lambda (expr env val)
         (conde
           ((clause* expr env val context))
           ...)))]))

(define eval-expo-with-default-ordering
  (eval-expo-specialization
    clause-quote
    clause-num
    clause-var
    clause-lambda
    clause-app-multi
    clause-app-variadic
    clause-cons
    clause-car
    clause-cdr
    clause-not
    clause-equal?
    clause-symbol?
    clause-null?
    handle-matcho
    clause-letrec
    boolean-primo
    and-primo
    or-primo
    if-primo))

(define eval-expo-if-test-ordering
    ;; bigrams from 44 Scheme programs:
    ;;
    ;; ((if-test null?) . 33)
    ;; ((if-test app) . 20)
    ;; ((if-test equal?) . 17)
    ;; ((if-test and) . 8)
    ;; ((if-test or) . 4)
    ;; ((if-test pair?) . 2)
    ;; ((if-test not) . 1)
    ;;
  (eval-expo-specialization
    clause-null?
    clause-equal?
    and-primo
    or-primo
    clause-app-multi
    clause-app-variadic
    clause-quote
    clause-num
    clause-var
    clause-lambda
    clause-cons
    clause-car
    clause-cdr
    clause-not
    clause-symbol?
    handle-matcho
    clause-letrec
    boolean-primo
    if-primo))

(define eval-expo-if-conseq-ordering
    ;; bigrams from 44 Scheme programs:
    ;;
    ;; ((if-conseq bool) . 24)
    ;; ((if-conseq nil) . 15)
    ;; ((if-conseq app) . 10)
    ;; ((if-conseq cons) . 10)
    ;; ((if-conseq var-arg) . 7)
    ;; ((if-conseq num) . 6)
    ;; ((if-conseq and) . 5)
    ;; ((if-conseq car) . 3)
    ;; ((if-conseq cdr) . 3)
    ;; ((if-conseq if) . 1)
    ;; ((if-conseq or) . 1)
    ;;
    (eval-expo-specialization
      boolean-primo
      clause-quote
      clause-num
      clause-var
      clause-cons
      clause-lambda
      clause-app-multi
      clause-app-variadic
      clause-car
      clause-cdr
      clause-not
      clause-equal?
      clause-symbol?
      clause-null?
      handle-matcho
      clause-letrec
      and-primo
      or-primo
      if-primo))

(define eval-expo-if-alt-ordering
  ;; bigrams from 44 Scheme programs:
  ;;
  ;; ((if-alt if) . 40)
  ;; ((if-alt app) . 21)
  ;; ((if-alt cons) . 17)
  ;; ((if-alt and) . 3)
  ;; ((if-alt bool) . 2)
  ;; ((if-alt equal?) . 1)
  ;; ((if-alt or) . 1)
  ;;
  (eval-expo-specialization
    clause-quote
    clause-num
    clause-var
    clause-lambda
    clause-cons
    if-primo
    clause-app-multi
    clause-app-variadic
    and-primo
    or-primo

    clause-car
    clause-cdr
    clause-not
    clause-equal?
    clause-symbol?
    clause-null?
    handle-matcho
    clause-letrec
    boolean-primo))


(define eval-expo-lambda-ordering
  ;; bigrams from 44 Scheme programs:
  ;;
  ;; ((lambda-multi if) . 43)
  ;; ((lambda-multi app) . 3)
  ;;
  (eval-expo-specialization
    clause-quote
    clause-num
    clause-var
    clause-lambda
    if-primo
    clause-app-multi
    clause-app-variadic
    clause-cons
    clause-car
    clause-cdr
    clause-not
    clause-equal?
    clause-symbol?
    clause-null?
    handle-matcho
    clause-letrec
    boolean-primo
    and-primo
    or-primo))


(define (eval-expo-app-rator-ordering context)
  (lambda (expr env val)
    ;; Cheat a little by requring the rator expression to be a variable reference.
    ;; Note that this prevents us from synthesizing code like:
    ;;
    ;; (((lambda (x) x) (lambda (x) x)) '(1 2 3))
    ;;
    ;; Variable lookup is by far the dominant case.
    ;; In most cases, the variable is a recursive call.
    ;; In any case, the variable should be bound to a *closure*.
    ;; old version: ((symbolo expr) (lookupo expr env val))
    ;;
    ;; This means we no longer need the 'conde'!
    (fresh (x body env^)
      (symbolo expr)
      (== `(closure (lambda ,x ,body) ,env^) val)
      (lookupo expr env val))))



(define (eval-expo-prim-app-rator-ordering context)
  (lambda (expr env val)
    ;; Cheat a little by requring the rator expression to be a variable reference.
    ;; Note that this prevents us from synthesizing code like:
    ;;
    ;; (((lambda (x) x) car) '(1 2 3))
    ;;
    ;; The variable should be bound to a *prim*.
    ;; old version: ((symbolo expr) (lookupo expr env val))
    ;;
    ;; This means we no longer need the 'conde'!
    (fresh (prim-id)
      (symbolo expr)
      (== `(prim . ,prim-id) val)
      (lookupo expr env val))))

(define eval-expo-app-rand*-ordering
  ;; bigrams from 44 Scheme programs:
  ;;
  ;; ((app-rand* var-arg) . 103)
  ;; ((app-rand* cdr) . 65)
  ;; ((app-rand* car) . 40)
  ;; ((app-rand* app) . 24)
  ;; ((app-rand* cons) . 3)
  ;; ((app-rand* nil) . 3)
  ;; ((app-rand* lambda) . 2)
  ;; ((app-rand* if) . 1)
  ;; ((app-rand* list) . 1)
  ;; ((app-rand* null?) . 1)
  ;;
  (eval-expo-specialization
    clause-var
    clause-cdr
    clause-car
    clause-app-multi
    clause-app-variadic
    clause-cons
    clause-quote
    clause-lambda
    clause-num
    clause-not
    clause-equal?
    clause-symbol?
    clause-null?
    handle-matcho
    clause-letrec
    boolean-primo
    and-primo
    or-primo
    if-primo))



(define (clause-quote expr env val context)
  (fresh ()
    (== `(quote ,val) expr)
    (absento 'closure val)
    (absento 'prim val)
    (not-in-envo 'quote env)))

(define (clause-num expr env val context)
  (fresh ()
    (numbero expr) (== expr val)))

(define (clause-var expr env val context)
  (fresh ()
    (symbolo expr) (lookupo expr env val)))

(define (clause-lambda expr env val context)
  (fresh (x body)
    (== `(lambda ,x ,body) expr)
    (== `(closure (lambda ,x ,body) ,env) val)
    (conde
      ;; Variadic
      ((symbolo x))
      ;; Multi-argument
      ((list-of-symbolso x)))
    (not-in-envo 'lambda env)))

(define (clause-app-multi expr env val context)
  (fresh (rator x* rands body env^ a* res)
    (== `(,rator . ,rands) expr)
    ;; Multi-argument
    ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
    (ext-env*o x* a* env^ res)
    (eval-application rands env a* ((make-eval-expo 'lambda-multi) body res val))))

(define (clause-app-variadic expr env val context)
  (fresh (rator x rands body env^ a* res)
    (== `(,rator . ,rands) expr)
    ;; variadic
    (symbolo x)
    (== `((,x . (val . ,a*)) . ,env^) res)
    ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
    ((make-eval-expo 'lambda-variadic) body res val)
    (eval-listo rands env a* 'app-rand*)))

(define (clause-cons expr env val context)
  (fresh (rator x* rands a* prim-id)
    (== `(,rator . ,rands) expr)
    ((make-eval-expo 'prim-app-rator)
     rator env `(prim . cons))
    (fresh (a d)
      (== `(,a ,d) a*)
      (== `(,a . ,d) val))
    (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-car expr env val context)
  (fresh (rator x* rands a* prim-id)
    (== `(,rator . ,rands) expr)
    ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
    (fresh (d)
      (== `((,val . ,d)) a*)
      (=/= 'closure val)
      (=/= 'prim val))
    (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-cdr expr env val context)
  (fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-not expr env val context)
  (fresh (rator x* rands a* prim-id)
    (== `(,rator . ,rands) expr)
    ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
    (fresh (b)
      (== `(,b) a*)
      (conde
        ((=/= #f b) (== #f val))
        ((== #f b) (== #t val))))
    (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-equal? expr env val context)
  (fresh (rator x* rands a* prim-id)
    (== `(,rator . ,rands) expr)
    ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
    (fresh (v1 v2)
      (== `(,v1 ,v2) a*)
      (conde
        ((== v1 v2) (== #t val))
        ((=/= v1 v2) (== #f val))))
    (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-symbol? expr env val context)
  (fresh (rator x* rands a* prim-id)
    (== `(,rator . ,rands) expr)
    ((make-eval-expo 'prim-app-rator) rator env `(prim . symbol?))
    (fresh (v)
      (== `(,v) a*)
      (conde
        ((symbolo v) (== #t val))
        ((numbero v) (== #f val))
        ((fresh (a d)
           (== `(,a . ,d) v)
           (== #f val)))))
    (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-null? expr env val context)
  (fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

(define (clause-letrec expr env val context)
  (fresh (p-name x body letrec-body)
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
       (=/= 'closure val)
       (=/= 'prim val))]
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
    ((symbolo t) (=/= 'closure t) (=/= 'prim t))
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
    (=/= 'prim mval)
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
       (=/= 'prim mval)
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
