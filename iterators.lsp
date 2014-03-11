(defpackage :cl-iterators
  (:use :common-lisp)
  (:export :iterator
           :next
           :take
           :take-while
           :take-until
           :zip
           :reset
           :make-iterator
           :with-iterator
           :to-string))

(in-package :cl-iterators)

(defclass stop-iteration-exception()
  ((init :initform (error "Stop iteration, iteration has exhausted."))))

(defclass iterator()
  ((start :initform 0 :initarg :start
          :documentation "The start of the iteration, either an index value or integer value to mark the start of iteration")
   (index :initform 0
          :documentation "the current index of the iteration") 
   (end :initform -1 :initarg :end
        :documentation "The end of the iteration, -1 denotes an infinite iteration, for lists/sequences, the sequence length limits the iteration")
   (increment :initform 1 :initarg :inc
              :documentation "the increment of the iteration")
   (id :initform #'identity :initarg :id
       :documentation "Applied over each value before returning to the caller.")
   (quiet :initform T :initarg :quiet
          :documentation "Whether to raise an error when iterations are exhausted")
   (cyclic :initform nil :initarg :cyclic
           :documentation "Whether the iteration is cyclic, in case where values are supplied the sequence re-iterated otherwise the iteration resets the index to start")
   (values :initform nil :initarg :values
           :documentation "A list/sequence of values to be iterated")
   (bound-check :initform #'<= :initarg :reverse
                :documentation "When T, direction of iteration is reversed")
   (comparer :initform #'(lambda(n values) (nth n values))
             :documentation "Function to access to individual members of a values, #'nth for lists, #'elt for others")))

(defmethod reset((iter iterator))
  (with-slots (start index) iter
    (setf index start)))

(defmethod next((iter iterator))
  (with-slots (index end increment id quiet cyclic
                     comparer bound-check values) iter
    (if (or (funcall bound-check index end) (eq end -1))
        (let ((current
               (funcall id (if values
                               (funcall comparer index values)
                               index))))
          (incf index increment)
          current)
        (if cyclic
            (progn
              (reset iter)
              (next iter))
            (when (not quiet)
              (error "Stop iteration, iterations have exhausted."))))))

(defmethod take(n (iter iterator))
  (loop for i from 1 to n
     for j = (next iter) 
     while j
     collect j))

(defmethod take-while(pred (iter iterator))
  (loop for i = (next iter)
     while (and i (funcall pred i))
     collect i))

(defmethod take-until(pred (iter iterator))
  (loop for i = (next iter)
     until (or (not i) (funcall pred i))
     collect i))

(defmethod zip((a iterator) (b iterator))
  (loop for i = (next a)
     for j = (next b) 
     while (and i j)
     collect (cons i j)))

(defmacro make-iterator(&key (start 0) (end -1) (inc 1)
                          (id #'identity) (values ()) (cyclic nil) (reverse nil))
  `(let ((tmp (make-instance 'iterator :start ,start
                             :end (if (null ,values)
                                      ,end
                                      (1- (length ,values)))
                             :inc ,inc :id ,id :values ,values :cyclic ,cyclic)))
     (with-slots (start end index values increment comparer bound-check) tmp
       (when (not (listp values))
         (setf comparer #'(lambda(n seq) (elt seq n))))
       (if ,reverse
         (setf start end
               index end
               end ,start
               bound-check #'>=
               increment (* -1 ,inc))
         (setf index start)))
     tmp))
         
(defmacro with-iterator((name &key (start 0) (end -1) (increment 1)
                              (values nil) (id #'identity)) &body body)
  `(let ((,name (make-iterator :start ,start
                               :end ,end :inc ,increment
                               :values ,values :id ,id)))
     ,@body))

(defmethod to-string((iter iterator))
  (with-slots (start end increment id quiet values cyclic) iter
    (format T "start: ~A, end: ~A, quiet: ~A, inc: ~A, cyclic: ~A, values: ~A ~%"
            start end quiet increment cyclic values)))
