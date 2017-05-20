(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

;; good ordering
(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== (cons a d) l)
         (== (cons a res) out)
         (appendo d s res))])))

#!eof

;; less good ordering (can cause infinite loops!)
(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (appendo d s res)
         (== (cons a d) l)
         (== (cons a res) out))])))

(define append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))
