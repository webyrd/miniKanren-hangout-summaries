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

(define (eval-expo-if-test-ordering context)
  (lambda (expr env val)
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
    (conde

      ;; null?
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ;; equal?
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
      
      ;; and
      ((and-primo expr env val context))

      ;; or
      ((or-primo expr env val context))

      ;; I'm putting application after the other common cases,
      ;; just because it is so expensive.
      ;; application
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))
      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
      
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
        
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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
      ((if-primo expr env val context))
    
      )))

(define (eval-expo-if-conseq-ordering context)
  (lambda (expr env val)
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
    (conde
      ;; bool
      ((boolean-primo expr env val context))

      ;; handles nil
      ((== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env))

      ((numbero expr) (== expr val))

      ((symbolo expr) (lookupo expr env val))

      ;; cons
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
      
      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (== `(closure (lambda ,x ,body) ,env) val)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))
    
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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

      ((and-primo expr env val context))
      ((or-primo expr env val context))
      ((if-primo expr env val context))
    
      )))

(define (eval-expo-if-alt-ordering context)
  (lambda (expr env val)
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

      ;; cons
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ;; if
      ((if-primo expr env val context))
            
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ;; and & or
      ((and-primo expr env val context))
      ((or-primo expr env val context))
      
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
    
      )))

(define (eval-expo-lambda-ordering context)
  (lambda (expr env val)
    ;; bigrams from 44 Scheme programs:
    ;;
    ;; ((lambda-multi if) . 43)
    ;; ((lambda-multi app) . 3)
    ;;    
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

      ;; if
      ((if-primo expr env val context))
      
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
    
      )))

(define (eval-expo-app-rator-ordering context)
  (lambda (expr env val)
    ;; Variable lookup is by far the dominant case.
    ;; In most cases, the variable is a recursive call.
    ;; In any case, the variable should be bound to a *closure*.
    ;; old version: ((symbolo expr) (lookupo expr env val))
    ;;
    ;; This means we no longer need the 'conde'!
    (fresh (x body env^)
      (symbolo expr)
      (== `(closure (lambda ,x ,body) ,env^) val)
      (lookupo expr env val))

    #|
    (conde
      ((symbolo expr)
       (fresh (x body env^)
         (== `(closure (lambda ,x ,body) ,env^) val)
         (lookupo expr env val)))

      ((== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env))

      ((numbero expr) (== expr val))

      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (== `(closure (lambda ,x ,body) ,env) val)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))
    
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
    
      )
    |#
    ))

(define (eval-expo-prim-app-rator-ordering context)
  (lambda (expr env val)
    ;; The variable should be bound to a *prim*.
    ;; old version: ((symbolo expr) (lookupo expr env val))
    ;;
    ;; This means we no longer need the 'conde'!
    (fresh (prim-id)
      (symbolo expr)
      (== `(prim . ,prim-id) val)
      (lookupo expr env val))

    #|
    (conde
      ((symbolo expr)
       (fresh (prim-id)
         (== `(prim . ,prim-id) val)
         (lookupo expr env val)))
      
      ((== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env))

      ((numbero expr) (== expr val))

      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (== `(closure (lambda ,x ,body) ,env) val)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))
    
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
    
      )
    |#
    
    ))

(define (eval-expo-app-rand*-ordering context)
  (lambda (expr env val)
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
    (conde
      ;; var
      ((symbolo expr) (lookupo expr env val))

      ;; cdr
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ;; car
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ;; application
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))

      ;; cons
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ;; handles nil
      ((== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env))

      ;; lambda
      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (== `(closure (lambda ,x ,body) ,env) val)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))

      ((numbero expr) (== expr val))

      
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
    
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; Multi-argument
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a* 'app-rand*)
         (ext-env*o x* a* env^ res)
         ((make-eval-expo 'lambda-multi) body res val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) expr)
         ;; variadic
         (symbolo x)
         (== `((,x . (val . ,a*)) . ,env^) res)
         ((make-eval-expo 'app-rator) rator env `(closure (lambda ,x ,body) ,env^))
         ((make-eval-expo 'lambda-variadic) body res val)
         (eval-listo rands env a* 'app-rand*)))
    
      #|
      ((fresh (rator x* rands a* prim-id)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env `(prim . ,prim-id))
      (eval-primo prim-id a* val)
      (eval-listo rands env a*)))
      |#

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator)
          rator env `(prim . cons))
         (fresh (a d)
           (== `(,a ,d) a*)
           (== `(,a . ,d) val))
         (eval-listo rands env a* 'prim-app-rand*)))
    
      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . car))
         (fresh (d)
           (== `((,val . ,d)) a*)
           (=/= 'closure val)
           (=/= 'prim val))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . cdr))
         (fresh (a)
           (== `((,a . ,val)) a*)
           (=/= 'closure a))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . not))
         (fresh (b)
           (== `(,b) a*)
           (conde
             ((=/= #f b) (== #f val))
             ((== #f b) (== #t val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . equal?))
         (fresh (v1 v2)
           (== `(,v1 ,v2) a*)
           (conde
             ((== v1 v2) (== #t val))
             ((=/= v1 v2) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))

      ((fresh (rator x* rands a* prim-id)
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

      ((fresh (rator x* rands a* prim-id)
         (== `(,rator . ,rands) expr)
         ((make-eval-expo 'prim-app-rator) rator env `(prim . null?))
         (fresh (v)
           (== `(,v) a*)
           (conde
             ((== '() v) (== #t val))
             ((=/= '() v) (== #f val))))
         (eval-listo rands env a* 'prim-app-rand*)))
    
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
