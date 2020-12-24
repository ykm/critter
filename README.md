critter
============

critter allows us to create an iterator over lists or sequences. Following functions have been included in the initial API

(next iter)
-----------
Gets the next element from the iterator. If the iterator is neither infinite nor cyclic and the iteration exhausts, the method throws up a stop-iteration-exception.

(take n iter)
-------------
Take n elements from the iterator. The return value is a list of the values accumulated. However, in future, we might need have to retain type of the initial-contents supplied during the iterator creation.

(take-while pred iter)
----------------------
take from iter while predicate 'pred' is true.

(zip iter1 iter2)
-----------------
zips two iterators, returns a list of cons with next value from each iterator, until one of them gets exhausted. Future plans are to perform this operation over multiple iterators at the same time.

Use cases
=========
Sequence Iterators
------------------
```
CL-USER> (defparameter foo (ct:make-iterator :initial-contents "hello" 
                                             :id #'(lambda(x) (cons x (char-code x)))))
FOO                                                                                                      
CL-USER> (ct:take 3 foo)
((#\h . 104) (#\e . 101) (#\l . 108))   
```

Cyclic iterators
----------------
```
CL-USER> (defparameter bar (ct:make-iterator :intial-contents '(1 2 3) :cyclic T))
BAR

CL-USER> (ct:take 20 bar)
(1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2)
```

Reverse iteration
-----------------
```
CL-USER> CL-USER> (defparameter baz (ct:make-iterator :intial-contents #(1 2 3 4) :from-end T))
BAZ
 
CL-USER> (ct:take 5 bar)
(4 3 2 1)
```

Iterate a list
--------------
```
CL-USER> (defvar baz (ct:make-iterator :initial-contents '(1 2 3 4 5)))
BAZ
         
CL-USER> (ct:take 2 baz)
(1 2)

CL-USER> (ct:take 2 baz)
(3 4)
```

Sequence generators(?)
-------------------
```
CL-USER> (defparameter *even-generator*                                                                    
           (ct:make-iterator :start 1 :id #'(lambda(x) (* 2 x))))
EVEN-GENERATOR                                                          

CL-USER> (ct:take 10 *even-generator*)
(2 4 6 8 10 12 14 16 18 20)   

CL-USER> (defvar *square-generator* (ct:make-iterator
                                           :start 0
                                           :inc 2
                                           :id #'(lambda(x) (* x x))))
                    
CL-USER> (ct:take 5 *square-generator*)
(0 4 16 36 64)                                                          
                                                      
CL-USER> (ct:take 5 *square-generator*)
(100 144 196 256 324)                                                                                                   
```

Upcoming
____________
1. Unit tests
2. Publishing to quicklisp