(load "mk-vicare.scm")
(load "mk.scm")
(load "full-interp.scm")

(run 1 (q f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([<= (lambda (l r)
                           (match (list l r)
                             [`(Z Z) #t]
                             [`(Z (S ,v)) #t]
                             [`((S ,v) Z) #f]
                             [`((S ,v1) (S ,v2)) (< v1 v2)]))])
              (letrec ([insert (lambda (x L <)
                                 (if (null? L) (list x)
                                     ((lambda (y M)
                                        (if (< x y)
                                            (cons x L)
                                            (cons y (insert x M <))))
                                      (car L) (cdr L))))])
                (letrec ([insertionsort (lambda (L <)
                                          (if (null? L) '()
                                              (insert (car L) (insertionsort (cdr L) <) <)))])
                  (equal? (insertionsort ',q (lambda (a b) (< (car a) (car b))))
                          (insertionsort ',q (lambda (a b) (<= (car a) (car b)))))))))
         #f))

;; (((((Z . _.0) (Z . _.1)) _.2)
;;    (=/= ((_.0 _.1)))
;;    (absento
;;      (closure _.0)
;;      (closure _.1)
;;      (prim _.0)
;;      (prim _.1))))





(run 1 (q f)
  (== `(< (car a) (car b)) f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (if (null? L) (list x)
                                    ((lambda (y M)
                                       (if (< x y)
                                           (cons x L)
                                           (cons y (insert1 x M <))))
                                     (car L) (cdr L))))])
              (letrec ([insert2 (lambda (x L <)
                                  (if (null? L) (list x)
                                      ((lambda (y M)
                                         (if (or (equal? x y) (< x y))
                                             (cons x L)
                                             (cons y (insert2 x M <))))
                                       (car L) (cdr L))))])
                (letrec ([insertionsort1 (lambda (L <)
                                           (if (null? L) '()
                                               (insert1 (car L) (insertionsort1 (cdr L) <) <)))])
                  (letrec ([insertionsort2 (lambda (L <)
                                             (if (null? L) '()
                                                 (insert2 (car L) (insertionsort2 (cdr L) <) <)))])
                    (equal? (insertionsort1 ',q (lambda (a b) ,f))
                            (insertionsort2 ',q (lambda (a b) ,f))))))))
         #f))

(run 1 (q f)
  (== `(< (car a) (car b)) f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (if (null? L) (list x)
                                    ((lambda (y M)
                                       (if (< x y)
                                           (cons x L)
                                           (cons y (insert1 x M <))))
                                     (car L) (cdr L))))])
              (letrec ([insert2 (lambda (x L <)
                                  (if (null? L) (list x)
                                      ((lambda (y M)
                                         (if (or (equal? x y) (< x y))
                                             (cons x L)
                                             (cons y (insert2 x M <))))
                                       (car L) (cdr L))))])
                (letrec ([insertionsort1 (lambda (< L)
                                           (if (null? L) '()
                                               (insert1 (car L) (insertionsort1 < (cdr L)) <)))])
                  (letrec ([insertionsort2 (lambda (< L)
                                             (if (null? L) '()
                                                 (insert2 (car L) (insertionsort2 < (cdr L)) <)))])
                    (equal? (insertionsort1 (lambda (a b) ,f) ',q)
                            (insertionsort2 (lambda (a b) ,f) ',q)))))))
         #f))



(run 1 (q f ac)
  (== `(< (,ac a) (,ac b)) f)  
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (if (null? L) (list x)
                                    ((lambda (y M)
                                       (if (< x y)
                                           (cons x L)
                                           (cons y (insert1 x M <))))
                                     (car L) (cdr L))))])
              (letrec ([insert2 (lambda (x L <)
                                  (if (null? L) (list x)
                                      ((lambda (y M)
                                         (if (or (equal? x y) (< x y))
                                             (cons x L)
                                             (cons y (insert2 x M <))))
                                       (car L) (cdr L))))])
                (letrec ([insertionsort1 (lambda (L <)
                                           (if (null? L) '()
                                               (insert1 (car L) (insertionsort1 (cdr L) <) <)))])
                  (letrec ([insertionsort2 (lambda (L <)
                                             (if (null? L) '()
                                                 (insert2 (car L) (insertionsort2 (cdr L) <) <)))])
                    (equal? (insertionsort1 ',q (lambda (a b) ,f))
                            (insertionsort2 ',q (lambda (a b) ,f))))))))
         #f))


(time (run 1 (q f ac)
        (== `((lambda (ac) (lambda (a b) (< (ac a) (ac b)))) ,ac) f)
        (evalo `(letrec ([< (lambda (l r)
                              (match (list l r)
                                [`(Z Z) #f]
                                [`(Z (S ,v)) #t]
                                [`((S ,v) Z) #f]
                                [`((S ,v1) (S ,v2)) (< v1 v2)]))])
                  (letrec ([insert1 (lambda (x L <)
                                      (if (null? L) (list x)
                                          ((lambda (y M)
                                             (if (< x y)
                                                 (cons x L)
                                                 (cons y (insert1 x M <))))
                                           (car L) (cdr L))))])
                    (letrec ([insert2 (lambda (x L <)
                                        (if (null? L) (list x)
                                            ((lambda (y M)
                                               (if (or (equal? x y) (< x y))
                                                   (cons x L)
                                                   (cons y (insert2 x M <))))
                                             (car L) (cdr L))))])
                      (letrec ([insertionsort1 (lambda (L <)
                                                 (if (null? L) '()
                                                     (insert1 (car L) (insertionsort1 (cdr L) <) <)))])
                        (letrec ([insertionsort2 (lambda (L <)
                                                   (if (null? L) '()
                                                       (insert2 (car L) (insertionsort2 (cdr L) <) <)))])
                          (equal? (insertionsort1 ',q ,f)
                                  (insertionsort2 ',q ,f)))))))
               #f)))

(time (run 1 (q f)
        (== 'car f)
        (evalo `(letrec ([< (lambda (l r)
                              (match (list l r)
                                [`(Z Z) #f]
                                [`(Z (S ,v)) #t]
                                [`((S ,v) Z) #f]
                                [`((S ,v1) (S ,v2)) (< v1 v2)]))])
                  (letrec ([<= (lambda (l r)
                                 (match (list l r)
                                   [`(Z Z) #t]
                                   [`(Z (S ,v)) #t]
                                   [`((S ,v) Z) #f]
                                   [`((S ,v1) (S ,v2)) (< v1 v2)]))])
                    (letrec ([insert1 (lambda (x L ac)
                                        (if (null? L) (list x)
                                            ((lambda (y M)
                                               (if (< (ac x) (ac y))
                                                   (cons x L)
                                                   (cons y (insert1 x M ac))))
                                             (car L) (cdr L))))])
                      (letrec ([insert2 (lambda (x L ac)
                                          (if (null? L) (list x)
                                              ((lambda (y M)
                                                 (if (<= (ac x) (ac y))
                                                     (cons x L)
                                                     (cons y (insert2 x M ac))))
                                               (car L) (cdr L))))])
                        (letrec ([insertionsort1 (lambda (L ac)
                                                   (if (null? L) '()
                                                       (insert1 (car L) (insertionsort1 (cdr L) ac) ac)))])
                          (letrec ([insertionsort2 (lambda (L ac)
                                                     (if (null? L) '()
                                                         (insert2 (car L) (insertionsort2 (cdr L) ac) ac)))])
                            (equal? (insertionsort1 ',q ,f)
                                    (insertionsort2 ',q ,f))))))))
               #f)))


(time (run 1 (q f)
        (== 'car f)
        (evalo `(letrec ([< (lambda (l r)
                              (match (list l r)
                                [`(Z Z) #f]
                                [`(Z (S ,v)) #t]
                                [`((S ,v) Z) #f]
                                [`((S ,v1) (S ,v2)) (< v1 v2)]))])
                  (letrec ([<= (lambda (l r)
                                 (match (list l r)
                                   [`(Z Z) #t]
                                   [`(Z (S ,v)) #t]
                                   [`((S ,v) Z) #f]
                                   [`((S ,v1) (S ,v2)) (< v1 v2)]))])
                    (letrec ([insert1 (lambda (x L ac)
                                        (if (null? L) (list x)
                                            ((lambda (y M)
                                               (if (< (ac x) (ac y))
                                                   (cons x L)
                                                   (cons y (insert1 x M ac))))
                                             (car L) (cdr L))))])
                      (letrec ([insert2 (lambda (x L ac)
                                          (if (null? L) (list x)
                                              ((lambda (y M)
                                                 (if (<= (ac x) (ac y))
                                                     (cons x L)
                                                     (cons y (insert2 x M ac))))
                                               (car L) (cdr L))))])
                        (letrec ([insertionsort1 (lambda (ac L)
                                                   (if (null? L) '()
                                                       (insert1 (car L) (insertionsort1 ac (cdr L)) ac)))])
                          (letrec ([insertionsort2 (lambda (ac L)
                                                     (if (null? L) '()
                                                         (insert2 (car L) (insertionsort2 ac (cdr L)) ac)))])
                            (equal? (insertionsort1 ,f ',q)
                                    (insertionsort2 ,f ',q))))))))
               #f)))












(run 1 (q f)
  (== `(< (car a) (car b)) f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (match L
                                  [`() (list x)]
                                  [`(,y . ,M)
                                   (if (< x y)
                                       (cons x L)
                                       (cons y (insert1 x M <)))]))])
              (letrec ([insert2 (lambda (x L <)
                                  (match L
                                    [`() (list x)]
                                    [`(,y . ,M)
                                     (if (or (equal? x y) (< x y))
                                         (cons x L)
                                         (cons y (insert2 x M <)))]))])
                (letrec ([insertionsort1 (lambda (< L)
                                           (match L
                                             [`() '()]
                                             [`(,a . ,d)
                                              (insert1 a (insertionsort1 < d) <)]))])
                  (letrec ([insertionsort2 (lambda (< L)
                                             (match L
                                               [`() '()]
                                               [`(,a . ,d)
                                                (insert2 a (insertionsort2 < d) <)]))])
                    (equal? (insertionsort1 (lambda (a b) ,f) ',q)
                            (insertionsort2 (lambda (a b) ,f) ',q)))))))
         #f))
;; (time (run 1 ...))
;;     14 collections
;;     0.237771000s elapsed cpu time, including 0.002642000s collecting
;;     0.239115000s elapsed real time, including 0.002674000s collecting
;;     117 265 088 bytes allocated, including 117875392 bytes reclaimed
;; (((((Z . _.0) (Z . _.1) (Z . _.0)) (< (car a) (car b)))
;;    (=/= ((_.0 _.1)))
;;    (absento
;;      (closure _.0)
;;      (closure _.1)
;;      (prim _.0)
;;      (prim _.1))))



(run 1 (q f)
  (== `(< (car a) (car b)) f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (match L
                                  [`() (list x)]
                                  [`(,y . ,M)
                                   (if (< x y)
                                       (cons x L)
                                       (cons y (insert1 x M <)))]))])
              (letrec ([insert2 (lambda (x L <)
                                  (match L
                                    [`() (list x)]
                                    [`(,y . ,M)
                                     (if (or (equal? x y) (< x y))
                                         (cons x L)
                                         (cons y (insert2 x M <)))]))])
                (letrec ([insertionsort1 (lambda (< L)                                           
                                           (if (null? L) '()
                                               (insert1 (car L) (insertionsort1 < (cdr L)) <)))])
                  (letrec ([insertionsort2 (lambda (< L)
                                             (if (null? L) '()
                                                 (insert2 (car L) (insertionsort2 < (cdr L)) <)))])
                    (equal? (insertionsort1 (lambda (a b) ,f) ',q)
                            (insertionsort2 (lambda (a b) ,f) ',q)))))))
         #f))
;; (time (run 1 ...))
;;     16 collections
;;     0.255733000s elapsed cpu time, including 0.003042000s collecting
;;     0.257274000s elapsed real time, including 0.003084000s collecting
;;     128 914 624 bytes allocated, including 134486880 bytes reclaimed
;; (((((Z . _.0) (Z . _.1) (Z . _.0)) (< (car a) (car b)))
;;    (=/= ((_.0 _.1)))
;;    (absento
;;      (closure _.0)
;;      (closure _.1)
;;      (prim _.0)
;;      (prim _.1))))


(run 1 (q f)
  (== `(< (car a) (car b)) f)
  (evalo `(letrec ([< (lambda (l r)
                        (match (list l r)
                          [`(Z Z) #f]
                          [`(Z (S ,v)) #t]
                          [`((S ,v) Z) #f]
                          [`((S ,v1) (S ,v2)) (< v1 v2)]))])
            (letrec ([insert1 (lambda (x L <)
                                (if (null? L) (list x)
                                    ((lambda (y M)
                                       (if (< x y)
                                           (cons x L)
                                           (cons y (insert1 x M <))))
                                     (car L) (cdr L))))])
              (letrec ([insert2 (lambda (x L <)
                                  (if (null? L) (list x)
                                      ((lambda (y M)
                                         (if (or (equal? x y) (< x y))
                                             (cons x L)
                                             (cons y (insert2 x M <))))
                                       (car L) (cdr L))))])
                (letrec ([insertionsort1 (lambda (< L)
                                           (if (null? L) '()
                                               (insert1 (car L) (insertionsort1 < (cdr L)) <)))])
                  (letrec ([insertionsort2 (lambda (< L)
                                             (if (null? L) '()
                                                 (insert2 (car L) (insertionsort2 < (cdr L)) <)))])
                    (equal? (insertionsort1 (lambda (a b) ,f) ',q)
                            (insertionsort2 (lambda (a b) ,f) ',q)))))))
         #f))
;; (time (run 1 ...))
;;     51 collections
;;     0.892428000s elapsed cpu time, including 0.010425000s collecting
;;     0.893513000s elapsed real time, including 0.010536000s collecting
;;     432 813 792 bytes allocated, including 429343968 bytes reclaimed
;; (((((Z . _.0) (Z . _.1) (Z . _.0)) (< (car a) (car b)))
;;    (=/= ((_.0 _.1)))
;;    (absento
;;      (closure _.0)
;;      (closure _.1)
;;      (prim _.0)
;;      (prim _.1))))








;; From Matt's blog post on Church encoding

;; Lists.

;; Lists are encoded as a match construct on lists.

; nil : list[*]
;; (define nil (lambda (on-null on-pair) 
;;               (on-null)))

;; ; kons : A list[A] -> list[A]
;; (define kons (lambda (a b)
;;                (lambda (on-null on-pair)
;;                  (on-pair a b))))

;; ; kar : list[A] -> A
;; (define kar (lambda (list)
;;               (list (lambda () (error))
;;                     (lambda (a b) a))))

;; ; kdr : list[A] -> list[A]
;; (define kdr (lambda (list)
;;               (list (lambda () (error))
;;                     (lambda (a b) b))))

;; ; kons? : list[A] -> boolean
;; (define kons? (lambda (list)
;;                 (list (lambda () #f)
;;                       (lambda (a b) #t))))

;; ; nil? : list[A] -> boolean
;; (define nil? (lambda (list)
;;                (list (lambda () #t)
;;                      (lambda (a b) #f))))


(run 1 (q)
  (evalo `(letrec ([error (lambda () (car '()))])
            (letrec ([nil?  (lambda (list)
                              (list (lambda () #t)
                                    (lambda (a b) #f)))])
              (letrec ([kar (lambda (list)
                              (list (lambda () (error))
                                    (lambda (a b) a)))])
                (letrec ([kdr (lambda (list)
                                (list (lambda () (error))
                                      (lambda (a b) b)))])
                  (and
                    (nil? (kdr (kdr ,q)))
                    (list (kar ,q) (kar (kdr ,q))))))))
         (list 5 6)))

(run 1 (q)
  (== `(lambda (a d)
         (d 5 (lambda (a d)
                (d 6 (lambda (a d)
                       (a))))))
      q)
  (evalo `(letrec ([error (lambda () (car '()))])
            (letrec ([nil?  (lambda (list)
                              (list (lambda () #t)
                                    (lambda (a b) #f)))])
              (letrec ([kar (lambda (list)
                              (list (lambda () (error))
                                    (lambda (a b) a)))])
                (letrec ([kdr (lambda (list)
                                (list (lambda () (error))
                                      (lambda (a b) b)))])
                  (and
                    (nil? (kdr (kdr ,q)))
                    (list (kar ,q) (kar (kdr ,q))))))))
         (list 5 6)))




















(time (run 1 (q x)
          (== `(lambda (a d)
                 (d 5 (lambda (a d)
                        ,x)))
              q)
          (evalo `(letrec ([error (lambda () (car '()))])
                    (letrec ([nil?  (lambda (list)
                                      (list (lambda () #t)
                                            (lambda (a b) #f)))])
                      (letrec ([kar (lambda (list)
                                      (list (lambda () (error))
                                            (lambda (a b) a)))])
                        (letrec ([kdr (lambda (list)
                                        (list (lambda () (error))
                                              (lambda (a b) b)))])
                          (and
                           (nil? (kdr (kdr ,q)))
                           (list (kar ,q) (kar (kdr ,q))))))))
                 (list 5 6))))
