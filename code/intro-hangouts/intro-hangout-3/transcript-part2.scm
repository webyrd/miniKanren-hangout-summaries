Chez Scheme Version 9.4.1
Copyright 1984-2016 Cisco Systems, Inc.

> (let ((x (+ 2 3)))
  (let ((x 7))
    x))
7
> (let ((x (+ 2 3)))
    (let ((x 7))
      x))
7
> 
;; ()
(let ((x (+ 2 3)))
  ;; ((x . 5))
  (let ((x 7))
    ;; ((x . 7) (x . 5))
    x))
7
> 
;; ()
(let ((x (+ 2 3)))
  ;; ((x . 5))
  (let ((y 7))
    ;; ((y . 7) (x . 5))
    y))
7
> 
;; ()
(let ((x (+ 2 3)))
  ;; ((x . 5))
  (let ((y864287467836487 7))  ;; alpha-renaming
    ;; ((y864287467836487 . 7) (x . 5))
    y864287467836487))
7
> (gensym "y")
#{y pfv8ch3j8hvgae47o3rqxdkce-0}
> y

Exception: variable y is not bound
Type (debug) to enter the debugger.
> (let ((x (+ 2 3)))
    (list
      (let ((x 7))
        x)
     x))
(7 5)
> (let ((x (+ 2 3)))
    (list
      (let ((x 7))
        x)
     x))