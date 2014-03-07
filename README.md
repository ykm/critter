cl-iterator
===========

Simple iterators with common lisp:

;; Infinite iterator
CL-USER> (defvar foo (cl-iterators:make-iterator 
                    :start 0 
                    :inc 2 
                    :id #'(lambda(x) (* x x))))

CL-USER> (cl-iterators:take 20 foo)
(0 4 16 36 64 100 144 196 256 324 400 484 576 676 784 900 1024 1156 1296 1444)

;; Finite iterator
CL-USER> (defvar bar (cl-iterators:make-iterator 
                    :start 0 
                    :inc 2 
                    :end 10
                    :id #'(lambda(x) (* x x))))

CL-USER> (cl-iterators:take 20 bar)
(0 4 16 36 64)

;; Iterate a list
CL-USER> (defvar baz (cl-iterators:make-iterator 
                     :values '(1 2 3 4 5)))

CL-USER> (cl-iterators:take 2 baz)
(1 2)

;; Cyclic iterators
CL-USER> (setq tmp (cl-iterators:make-iterator 
                    :values '(1 2 3)
                    :cyclic T))

CL-USER> (cl-iterators:take 20 tmp)
(1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2)

;; Other functions
;; 1. take-while/take-until
;; 2. zip: zips two iterators

;; Macros: make-iterator, with-iterator










