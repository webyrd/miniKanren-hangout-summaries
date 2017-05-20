(load "mk-vicare.scm")
(load "mk.scm")
(load "full-interp.scm")
(load "test-check.scm")

;; append tests

(time (test "append-0"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(_.0)))

(time (test "append-1"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           q))
        '((a b c d e))))

(time (test "append-2"
        (run 2 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ,q '(d e)))
           '(a b c d e)))
        '('(a b c)
          (((lambda _.0 '(a b c))) (=/= ((_.0 quote))) (sym _.0)))))

(time (test "append-3"
        (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q '(d e)))
           '(a b c d e)))
        '((a b c))))

(time (test "append-4"
        (run* (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append ',q ',r))
           '(a b c d e)))
        '((() (a b c d e))
          ((a) (b c d e))
          ((a b) (c d e))
          ((a b c) (d e))
          ((a b c d) (e))
          ((a b c d e) ()))))

(time (test "append-5"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons ,q (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((car l))))

(time (test "append-6"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr ,q) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(l)))

(time (test "append-7"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '(cdr)))

(time (test "append-8"
        (run 1 (q r)
          (symbolo q)
          (symbolo r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((cdr l))))

;;; doesn't seem to come back
#|
(time (test "append-9"
        (run 1 (q r)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (,q ,r) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((cdr l))))
|#

(time (test "append-10"
        (run 1 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if ,q
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((equal? l '()))))

#|
run 2 takes 19 seconds, got tired of waiting for run 3
(time (test "append-11"
        (run 4 (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if ,q
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
        '((equal? l '())
          (null? l)
          ((((lambda _.0 null?)) l) (=/= ((_.0 null?))) (sym _.0))
          (equal? '() l))))
|#

;; We use the relational Racket interpreter, extended to support 'and'
;; and 'or', to allow us to write a simple proof checker for
;; propositional logic as a Racket function.  Because we can treat the
;; Racket function as a relation, this proof *checker* can act as a
;; theorem prover, finding a proof tree to prove a theorem.

;; The simple proof checker and proof example are from Matt Might.



;; The proof checker uses 'and', so we have added both 'and' and 'or'
;; to the relational interpreter.  We can't just add 'and' as a helper
;; function, as we do with 'member?', since 'and' uses short-circuit
;; evaluation.

;; Let's test 'and' and 'or':

;; and tests
(time (test "and-0"
        (run* (q) (evalo '(and) q))
        '(#t)))

(time (test "and-1"
        (run* (q) (evalo '(and 5) q))
        '(5)))

(time (test "and-2"
        (run* (q) (evalo '(and #f) q))
        '(#f)))

(time (test "and-3"
        (run* (q) (evalo '(and 5 6) q))
        '(6)))

(test "and-4"
  (run* (q) (evalo '(and #f 6) q))
  '(#f))

(test "and-5"
  (run* (q) (evalo '(and (null? '()) 6) q))
  '(6))

(test "and-6"
  (run* (q) (evalo '(and (null? '(a b c)) 6) q))
  '(#f))


;; or tests
(test "or-0"
  (run* (q) (evalo '(or) q))
  '(#f))

(test "or-1"
  (run* (q) (evalo '(or 5) q))
  '(5))

(test "or-2"
  (run* (q) (evalo '(or #f) q))
  '(#f))

(test "or-3"
  (run* (q) (evalo '(or 5 6) q))
  '(5))

(test "or-4"
  (run* (q) (evalo '(or #f 6) q))
  '(6))

(test "or-5"
  (run* (q) (evalo '(or (null? '()) 6) q))
  '(#t))

(test "or-6"
  (run* (q) (evalo '(or (null? '(a b c)) 6) q))
  '(6))


;; We now port Matt Might's proof checker to use the subset of Racket
;; supported by our relational interpreter.  Our example problem is
;; also from Matt.
;;
;; Matt's minimalist proof checker for propositional logic:

#|
(define (proof? proof)
  (match proof
    ((assumption ,assms () ,A) (member? A assms))
    ((modus-ponens
      ,assms (,(and ant1 ‘(,_ ,assms1 ,_ (if ,A ,B)))
              ,(and ant2 ‘(,_ ,assms2 ,_ ,C))) ,D)
     (and (equal? A C) (equal? B D)
          (equal? assms assms1) (equal? assms assms2)
          (proof? ant1)
          (proof? ant2)))))
|#

;; Here is our port of the proof checker to our interpreter.  We use
;; 'letrec' instead of 'define', we define 'member?' as a helper
;; function, and use Racket's pattern-matching syntax.  The resulting
;; 'letrec' expression runs without modification in Racket, since the
;; expression does not include any logic variables.

;; We are asking the proof checker to check our proof of C, using the
;; assumptions A, A => B, and B => C.  Note that we give the entire
;; proof tree as the input to 'proof?'.

;; 4 collections
;; 3980 ms elapsed cpu time, including 0 ms collecting
;; 3985 ms elapsed real time, including 0 ms collecting
;; 33762080 bytes allocated
(time (test "proof-1"
  (run* (q)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (letrec ((proof? (lambda (proof)
                           (match proof
                             [`(assumption ,assms () ,A)
                              (member? A assms)]
                             [`(modus-ponens
                                ,assms
                                ((,r1 ,assms ,ants1 (if ,A ,B))
                                 (,r2 ,assms ,ants2 ,A))
                                ,B)
                              (and (proof? (list r1 assms ants1 (list 'if A B)))
                                   (proof? (list r2 assms ants2 A)))]))))
          (proof? '(modus-ponens
                     (A (if A B) (if B C))
                     ((assumption (A (if A B) (if B C)) () (if B C))
                      (modus-ponens
                        (A (if A B) (if B C))
                        ((assumption (A (if A B) (if B C)) () (if A B))
                         (assumption (A (if A B) (if B C)) () A)) B))
                     C))))
     q))
  '(#t)))

;; Getting ready to run the proof checker as a theorem prover.  To
;; make sure our query has the right syntactic structure, we unify
;; 'prf' with the answer.  So we are still running the proof checker
;; "forwards," although we are using logic variables, so this code
;; doesn't run directly in Racket.

;; 3 collections
;; 3478 ms elapsed cpu time, including 0 ms collecting
;; 3480 ms elapsed real time, including 0 ms collecting
;; 23896992 bytes allocated
(time (test "proof-2a"
  (run* (prf)
    (fresh (rule assms ants)
      (== '(modus-ponens
             (A (if A B) (if B C))
             ((assumption (A (if A B) (if B C)) () (if B C))
              (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
             C)
          prf)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C))))

;; Another test to ensure we are instantiating 'prf' and 'assms' to
;; the correct terms before we try running the proof checker as a
;; theorem prover.  Once again, this test runs forwards.

;; 3 collections
;; 3352 ms elapsed cpu time, including 0 ms collecting
;; 3356 ms elapsed real time, including 0 ms collecting
;; 23833552 bytes allocated
(time (test "proof-2b"
  (run* (prf)
    (fresh (rule assms ants)
      (== `(,rule ,assms ,ants C) prf)
      (== `(A (if A B) (if B C)) assms)
      (== '(modus-ponens
             (A (if A B) (if B C))
             ((assumption (A (if A B) (if B C)) () (if B C))
              (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
             C)
          prf)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C))))

;; The real test!  We are no longer unifying 'prf' with the answer.
;; The proof checker is now inferring the proof tree for the theorem
;; we are trying to prove (C) given a set of assumptions (A, A => B,
;; and B => C).  The proof checker *function* is now acting as a
;; *relation*, which lets us use it as a theorem prover.

;; 10 collections
;; 12273 ms elapsed cpu time, including 1 ms collecting
;; 12283 ms elapsed real time, including 2 ms collecting
;; 82533568 bytes allocated
;;
;; run 2 seems to diverge
(time (test "proof-2c"
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C))))

#!eof

;; Here we run the proof checker/theorem prover with a fresh logic variable
;; representing the proof tree.  This allows us to generate valid
;; proof trees, where each proof tree contains a theorem and the
;; assumptions used to prove that theorem.
;;
;; From the answers it is clear the prover tends to generate "proofs
;; by assumption", assuming the theorem to be proved.  This isn't
;; surprising, since such proofs require relatively little
;; computation.  A couple of the proof trees do use modus ponens,
;; however.
;;
;; 18 collections
;; 45118 ms elapsed cpu time, including 3 ms collecting
;; 45137 ms elapsed real time, including 4 ms collecting
;; 150564400 bytes allocated
(test "generate-theorems/proofs"
  (run 20 (prf)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (letrec ((proof? (lambda (proof)
                           (match proof
                             [`(assumption ,assms () ,A)
                              (member? A assms)]
                             [`(modus-ponens
                                ,assms
                                ((,r1 ,assms ,ants1 (if ,A ,B))
                                 (,r2 ,assms ,ants2 ,A))
                                ,B)
                              (and (proof? (list r1 assms ants1 (list 'if A B)))
                                   (proof? (list r2 assms ants2 A)))]))))
          (proof? ',prf)))
     #t))
  '(((assumption (_.0 . _.1) () _.0)
     (absento (closure _.0) (closure _.1)))
    ((assumption (_.0 _.1 . _.2) () _.1) (=/= ((_.0 _.1)))
     (absento (closure _.0) (closure _.1) (closure _.2)))
    ((assumption (_.0 _.1 _.2 . _.3) () _.2)
     (=/= ((_.0 _.2)) ((_.1 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)))
    ((assumption (_.0 _.1 _.2 _.3 . _.4) () _.3)
     (=/= ((_.0 _.3)) ((_.1 _.3)) ((_.2 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 . _.5) () _.4)
     (=/= ((_.0 _.4)) ((_.1 _.4)) ((_.2 _.4)) ((_.3 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 . _.6) () _.5)
     (=/= ((_.0 _.5)) ((_.1 _.5)) ((_.2 _.5)) ((_.3 _.5))
          ((_.4 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 . _.7) () _.6)
     (=/= ((_.0 _.6)) ((_.1 _.6)) ((_.2 _.6)) ((_.3 _.6))
          ((_.4 _.6)) ((_.5 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 . _.8) () _.7)
     (=/= ((_.0 _.7)) ((_.1 _.7)) ((_.2 _.7)) ((_.3 _.7))
          ((_.4 _.7)) ((_.5 _.7)) ((_.6 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 . _.9) () _.8)
     (=/= ((_.0 _.8)) ((_.1 _.8)) ((_.2 _.8)) ((_.3 _.8))
          ((_.4 _.8)) ((_.5 _.8)) ((_.6 _.8)) ((_.7 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 . _.10) () _.9)
     (=/= ((_.0 _.9)) ((_.1 _.9)) ((_.2 _.9)) ((_.3 _.9))
          ((_.4 _.9)) ((_.5 _.9)) ((_.6 _.9)) ((_.7 _.9))
          ((_.8 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 . _.11)
      () _.10)
     (=/= ((_.0 _.10)) ((_.1 _.10)) ((_.10 _.2)) ((_.10 _.3))
          ((_.10 _.4)) ((_.10 _.5)) ((_.10 _.6)) ((_.10 _.7))
          ((_.10 _.8)) ((_.10 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 . _.12)
      () _.11)
     (=/= ((_.0 _.11)) ((_.1 _.11)) ((_.10 _.11))
          ((_.11 _.2)) ((_.11 _.3)) ((_.11 _.4)) ((_.11 _.5))
          ((_.11 _.6)) ((_.11 _.7)) ((_.11 _.8)) ((_.11 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12 . _.13)
      () _.12)
     (=/= ((_.0 _.12)) ((_.1 _.12)) ((_.10 _.12))
          ((_.11 _.12)) ((_.12 _.2)) ((_.12 _.3)) ((_.12 _.4))
          ((_.12 _.5)) ((_.12 _.6)) ((_.12 _.7)) ((_.12 _.8))
          ((_.12 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12 _.13 . _.14)
      () _.13)
     (=/= ((_.0 _.13)) ((_.1 _.13)) ((_.10 _.13))
          ((_.11 _.13)) ((_.12 _.13)) ((_.13 _.2)) ((_.13 _.3))
          ((_.13 _.4)) ((_.13 _.5)) ((_.13 _.6)) ((_.13 _.7))
          ((_.13 _.8)) ((_.13 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((modus-ponens ((if _.0 _.1) _.0 . _.2)
                   ((assumption ((if _.0 _.1) _.0 . _.2) () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.0 . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 . _.15)
      () _.14)
     (=/= ((_.0 _.14)) ((_.1 _.14)) ((_.10 _.14))
          ((_.11 _.14)) ((_.12 _.14)) ((_.13 _.14)) ((_.14 _.2))
          ((_.14 _.3)) ((_.14 _.4)) ((_.14 _.5)) ((_.14 _.6))
          ((_.14 _.7)) ((_.14 _.8)) ((_.14 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 . _.16)
      () _.15)
     (=/= ((_.0 _.15)) ((_.1 _.15)) ((_.10 _.15))
          ((_.11 _.15)) ((_.12 _.15)) ((_.13 _.15))
          ((_.14 _.15)) ((_.15 _.2)) ((_.15 _.3)) ((_.15 _.4))
          ((_.15 _.5)) ((_.15 _.6)) ((_.15 _.7)) ((_.15 _.8))
          ((_.15 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 _.16 . _.17)
      () _.16)
     (=/= ((_.0 _.16)) ((_.1 _.16)) ((_.10 _.16))
          ((_.11 _.16)) ((_.12 _.16)) ((_.13 _.16))
          ((_.14 _.16)) ((_.15 _.16)) ((_.16 _.2)) ((_.16 _.3))
          ((_.16 _.4)) ((_.16 _.5)) ((_.16 _.6)) ((_.16 _.7))
          ((_.16 _.8)) ((_.16 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.17) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 _.16 _.17 . _.18)
      () _.17)
     (=/= ((_.0 _.17)) ((_.1 _.17)) ((_.10 _.17))
          ((_.11 _.17)) ((_.12 _.17)) ((_.13 _.17))
          ((_.14 _.17)) ((_.15 _.17)) ((_.16 _.17)) ((_.17 _.2))
          ((_.17 _.3)) ((_.17 _.4)) ((_.17 _.5)) ((_.17 _.6))
          ((_.17 _.7)) ((_.17 _.8)) ((_.17 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.17) (closure _.18) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((modus-ponens ((if _.0 _.1) _.2 _.0 . _.3)
                   ((assumption ((if _.0 _.1) _.2 _.0 . _.3) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.0 . _.3) () _.0))
                   _.1)
     (=/= ((_.0 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))))


;; Since the proof checker/theorem prover tends to generate trivial
;; proof trees that just assume the theorem to be proved, lets
;; restrict the outer proof rule to be modus ponens.
;;
;; 27 collections
;; 84672 ms elapsed cpu time, including 7 ms collecting
;; 84794 ms elapsed real time, including 7 ms collecting
;; 226336768 bytes allocated
(test "generate-theorems/proofs-using-modus-ponens"
  (run 20 (prf)
    (fresh (assms ants conseq)
      (== `(modus-ponens ,assms ,ants ,conseq) prf)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '(((modus-ponens ((if _.0 _.1) _.0 . _.2)
                   ((assumption ((if _.0 _.1) _.0 . _.2) () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.0 . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)))
    ((modus-ponens ((if _.0 _.1) _.2 _.0 . _.3)
                   ((assumption ((if _.0 _.1) _.2 _.0 . _.3) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.0 . _.3) () _.0))
                   _.1)
     (=/= ((_.0 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((modus-ponens (_.0 (if _.0 _.1) . _.2)
                   ((assumption (_.0 (if _.0 _.1) . _.2) () (if _.0 _.1))
                    (assumption (_.0 (if _.0 _.1) . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.0 . _.4)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.0 . _.4) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.0 . _.4) () _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5) ()
                                _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                                () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                                () _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7) ()
        (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7) () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7)))
    ((modus-ponens (_.0 (if _.1 _.2) _.1 . _.3)
                   ((assumption (_.0 (if _.1 _.2) _.1 . _.3) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.1 . _.3) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((modus-ponens (_.0 _.1 (if _.0 _.2) . _.3)
                   ((assumption (_.0 _.1 (if _.0 _.2) . _.3) ()
                                (if _.0 _.2))
                    (assumption (_.0 _.1 (if _.0 _.2) . _.3) () _.0))
                   _.2)
     (=/= ((_.1 (if _.0 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8) ()
        (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8) ()
        _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0 . _.9)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0 . _.9)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0
         . _.9)
        () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.1 . _.4)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.1 . _.4) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.1 . _.4) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
       . _.10)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
         . _.10)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
         . _.10)
        () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.0
       . _.11)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.0 . _.11)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.0 . _.11)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4))
          ((_.0 _.5)) ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8))
          ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5) ()
                                _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3))
          ((_.1 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)))
    ((modus-ponens (_.0 _.1 (if _.1 _.2) . _.3)
                   ((assumption (_.0 _.1 (if _.1 _.2) . _.3) ()
                                (if _.1 _.2))
                    (assumption (_.0 _.1 (if _.1 _.2) . _.3) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
       _.0 . _.12)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.0 . _.12)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.0 . _.12)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.11)) ((_.0 _.2)) ((_.0 _.3))
          ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6)) ((_.0 _.7))
          ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
       _.12 _.0 . _.13)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.0 . _.13)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.0 . _.13)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.11)) ((_.0 _.12)) ((_.0 _.2))
          ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6))
          ((_.0 _.7)) ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                                () (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                                () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3))
          ((_.1 _.4)) ((_.1 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6)))
    ((modus-ponens ((if _.0 _.0) _.0 . _.1)
                   ((assumption ((if _.0 _.0) _.0 . _.1) () (if _.0 _.0))
                    (modus-ponens ((if _.0 _.0) _.0 . _.1)
                                  ((assumption ((if _.0 _.0) _.0 . _.1) ()
                                               (if _.0 _.0))
                                   (assumption ((if _.0 _.0) _.0 . _.1) () _.0))
                                  _.0))
                   _.0)
     (absento (closure _.0) (closure _.1)))))

;; Here we generate *incorrect* proof trees.  That is, proof trees
;; that *do not* prove the theorem from the given set of assumptions.
;; We do this simply by changing the last argument of 'evalo' to
;; #f instead of #t.  In other words, we are inferring proofs for
;; which the 'proof?' function in Racket would return #f.

;; 15 collections
;; 29688 ms elapsed cpu time, including 2 ms collecting
;; 29691 ms elapsed real time, including 2 ms collecting
;; 120117040 bytes allocated
(test "generate-non-theorems/proofs"
  (run 20 (prf)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (letrec ((proof? (lambda (proof)
                           (match proof
                             [`(assumption ,assms () ,A)
                              (member? A assms)]
                             [`(modus-ponens
                                ,assms
                                ((,r1 ,assms ,ants1 (if ,A ,B))
                                 (,r2 ,assms ,ants2 ,A))
                                ,B)
                              (and (proof? (list r1 assms ants1 (list 'if A B)))
                                   (proof? (list r2 assms ants2 A)))]))))
          (proof? ',prf)))
     #f))
  '(((assumption () () _.0) (absento (closure _.0)))
    ((assumption (_.0) () _.1) (=/= ((_.0 _.1)))
     (absento (closure _.0) (closure _.1)))
    ((assumption (_.0 _.1) () _.2)
     (=/= ((_.0 _.2)) ((_.1 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)))
    ((assumption (_.0 _.1 _.2) () _.3)
     (=/= ((_.0 _.3)) ((_.1 _.3)) ((_.2 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((assumption (_.0 _.1 _.2 _.3) () _.4)
     (=/= ((_.0 _.4)) ((_.1 _.4)) ((_.2 _.4)) ((_.3 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4)))
    ((assumption (_.0 _.1 _.2 _.3 _.4) () _.5)
     (=/= ((_.0 _.5)) ((_.1 _.5)) ((_.2 _.5)) ((_.3 _.5))
          ((_.4 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5) () _.6)
     (=/= ((_.0 _.6)) ((_.1 _.6)) ((_.2 _.6)) ((_.3 _.6))
          ((_.4 _.6)) ((_.5 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6)))
    ((modus-ponens ()
       ((assumption () () (if _.0 _.1)) (_.2 () _.3 _.0)) _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6) () _.7)
     (=/= ((_.0 _.7)) ((_.1 _.7)) ((_.2 _.7)) ((_.3 _.7))
          ((_.4 _.7)) ((_.5 _.7)) ((_.6 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7) () _.8)
     (=/= ((_.0 _.8)) ((_.1 _.8)) ((_.2 _.8)) ((_.3 _.8))
          ((_.4 _.8)) ((_.5 _.8)) ((_.6 _.8)) ((_.7 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8) () _.9)
     (=/= ((_.0 _.9)) ((_.1 _.9)) ((_.2 _.9)) ((_.3 _.9))
          ((_.4 _.9)) ((_.5 _.9)) ((_.6 _.9)) ((_.7 _.9))
          ((_.8 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9) ()
                 _.10)
     (=/= ((_.0 _.10)) ((_.1 _.10)) ((_.10 _.2)) ((_.10 _.3))
          ((_.10 _.4)) ((_.10 _.5)) ((_.10 _.6)) ((_.10 _.7))
          ((_.10 _.8)) ((_.10 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((modus-ponens (_.0)
                   ((assumption (_.0) () (if _.1 _.2)) (_.3 (_.0) _.4 _.1))
                   _.2)
     (=/= ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10) () _.11)
     (=/= ((_.0 _.11)) ((_.1 _.11)) ((_.10 _.11))
          ((_.11 _.2)) ((_.11 _.3)) ((_.11 _.4)) ((_.11 _.5))
          ((_.11 _.6)) ((_.11 _.7)) ((_.11 _.8)) ((_.11 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11) ()
      _.12)
     (=/= ((_.0 _.12)) ((_.1 _.12)) ((_.10 _.12))
          ((_.11 _.12)) ((_.12 _.2)) ((_.12 _.3)) ((_.12 _.4))
          ((_.12 _.5)) ((_.12 _.6)) ((_.12 _.7)) ((_.12 _.8))
          ((_.12 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12)
      () _.13)
     (=/= ((_.0 _.13)) ((_.1 _.13)) ((_.10 _.13))
          ((_.11 _.13)) ((_.12 _.13)) ((_.13 _.2)) ((_.13 _.3))
          ((_.13 _.4)) ((_.13 _.5)) ((_.13 _.6)) ((_.13 _.7))
          ((_.13 _.8)) ((_.13 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13)
      () _.14)
     (=/= ((_.0 _.14)) ((_.1 _.14)) ((_.10 _.14))
          ((_.11 _.14)) ((_.12 _.14)) ((_.13 _.14)) ((_.14 _.2))
          ((_.14 _.3)) ((_.14 _.4)) ((_.14 _.5)) ((_.14 _.6))
          ((_.14 _.7)) ((_.14 _.8)) ((_.14 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9)))
    ((modus-ponens (_.0 _.1)
                   ((assumption (_.0 _.1) () (if _.2 _.3))
                    (_.4 (_.0 _.1) _.5 _.2))
                   _.3)
     (=/= ((_.0 (if _.2 _.3))) ((_.1 (if _.2 _.3))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14)
      () _.15)
     (=/= ((_.0 _.15)) ((_.1 _.15)) ((_.10 _.15))
          ((_.11 _.15)) ((_.12 _.15)) ((_.13 _.15))
          ((_.14 _.15)) ((_.15 _.2)) ((_.15 _.3)) ((_.15 _.4))
          ((_.15 _.5)) ((_.15 _.6)) ((_.15 _.7)) ((_.15 _.8))
          ((_.15 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15)
      () _.16)
     (=/= ((_.0 _.16)) ((_.1 _.16)) ((_.10 _.16))
          ((_.11 _.16)) ((_.12 _.16)) ((_.13 _.16))
          ((_.14 _.16)) ((_.15 _.16)) ((_.16 _.2)) ((_.16 _.3))
          ((_.16 _.4)) ((_.16 _.5)) ((_.16 _.6)) ((_.16 _.7))
          ((_.16 _.8)) ((_.16 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9)))))
