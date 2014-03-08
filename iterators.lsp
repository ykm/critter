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
  ((start :initform 0 :initarg :start :accessor :iterator-start)
   (index :initform 0 :accessor :iterator-index)
   (end :initform -1 :initarg :end :accessor :iterator-end)
   (increment :initform 1 :initarg :inc :accessor :iterator-inc)
   (id :initform #'identity :initarg :id)
   (quiet :initform T :initarg :quiet)
   (cyclic :initform nil :initarg :cyclic)
   (values :initform nil :initarg :values)))

(defmethod reset((iter iterator))
  (with-slots (start index) iter
    (setf index start)))

(defmethod next((iter iterator))
  (with-slots (index end increment id quiet values cyclic) iter
    (if (or (<= index end) (eq end -1))
        (let ((current
               (funcall id (if values
                               (nth index values)
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
                          (id #'identity) (values ()) (cyclic Nil))
  `(let ((tmp (make-instance 'iterator :start ,start
                             :end (if (null ,values)
                                      ,end
                                      (1- (length ,values)))
                             :inc ,inc :id ,id :values ,values :cyclic ,cyclic)))
     (setf (slot-value tmp 'index) ,start)
     tmp))
         
(defmacro with-iterator((name &key (start 0) (end -1) (increment 1)
                              (values nil) (id #'identity)) &body body)
  `(let ((,name (make-iterator :start ,start
                               :end ,end :inc ,increment
                               :values ,values :id ,id)))
     ,@body))

(defmethod to-string((iter iterator))
  (with-slots (start end increment id quiet values cyclic) iter
    (format nil "start: ~A, end: ~A, quiet: ~A, inc: ~A, cyclic: ~A, values: ~{~A~^, ~}"
            start end quiet increment cyclic values)))
