(defpackage :cl-iterators
  (:use :common-lisp)
  (:nicknames :iter)
  (:export :iterator
           :next
           :take
           :take-while
           :zip
           :reset
           :cyclic-p
           :exhausted-p
           :make-iterator
           :with-iterator
           :do-sequence-iterator
           :do-sequence-iterators
           :do-sequence-iterators*
           :dosequences*
           :dosequence
           :list-to-alist
           :to-string))

(in-package :cl-iterators)

(defvar *limit* most-positive-fixnum
  "Serves as the end of the iterator in case where it isn't specified.")

(define-condition stop-iteration-exception()
  ((text :initarg :text
         :reader text)))

(defclass iterator()
  ((start :initform 0
          :initarg :start
          :accessor start
          :documentation "The start of the iteration, either an index value or integer value to mark the start of iteration")
   (index :initform 0
          :accessor index
          :documentation "the current index of the iteration") 
   (end :initform *limit*
        :initarg :end
        :accessor end
        :documentation "The end of the iteration, *limit* denotes an infinite iteration, for lists/sequences, the sequence length limits the iteration")
   (increment :initform 1
              :initarg :inc
              :accessor increment
              :documentation "the increment of the iteration")
   (id :initform #'identity
       :initarg :id
       :accessor id
       :documentation "Applied over each value before returning to the caller.")
   (quiet :initform nil
          :initarg :quiet
          :accessor quiet-p
          :documentation "Whether to raise an error when iterations are exhausted")
   (contents :initform nil
             :initarg :initial-contents
             :accessor initial-contents
             :documentation "A list/sequence of initial-contents to be iterated")
   (element :initform nil
            :initarg :initial-element
            :accessor initial-element
            :documentation "When supplied and no initial-contents supplied, iteration returns the value instead of the index")
   (cyclic :initform nil
           :initarg :cyclic
           :accessor cyclic-p
           :documentation "Whether the iteration is cyclic, in case where initial-contents are supplied the sequence re-iterated otherwise the iteration resets the index to start")
   (check :initform #'<=
          :accessor bound-check
          :documentation "Function to check the boundaries of the iterator.")
   (from-end :initarg :from-end
             :accessor from-end-p
             :documentation "When T, direction of iteration is reversed.")
   (accessor :initform #'elt
             :accessor accessor
             :documentation "Function to access to individual members of a initial-contents, #'nth for lists, #'elt for others")
   (exhausted :initform nil
              :accessor exhausted-p
              :documentation "A flag which is set when the iterator exhausts.")))

(defmethod initialize-instance :after ((iter iterator) &key)
  (with-accessors ((start start) (index index) (end end)
                   (from-end from-end-p) (inc increment)
                   (accessor accessor) (check bound-check)
                   (contents initial-contents)) iter
    (when contents
      (let ((len (1- (length contents))))
        (cond
          ((= end *limit*) (setf end len))
          ((> end len) (error "length out of bounds"))))
      (when (listp contents)
        (setf accessor #'(lambda(seq n) (nth n seq)))))
    (when from-end
      (rotatef start end)
      (setf index start
            check #'>=
            inc (* -1 inc)))
    (setf index start)))

(defmethod reset ((iter iterator))
  (with-accessors ((start start) (index index)
                   (exhausted exhausted-p)) iter
    (setf exhausted nil)
    (setf index start)))

(defmethod valid-p ((iter iterator))
  (with-accessors ((index index)
                   (check bound-check)
                   (end end)) iter
    (or (eq end *limit*) (funcall check index end))))

(defmethod to-string ((iter iterator))
  (with-slots (start end increment id quiet contents cyclic) iter
    (format nil "start: ~A, end: ~A, quiet: ~A, inc: ~A, cyclic: ~A, initial-contents: ~A ~%"
            start end quiet increment cyclic contents)))

(defmethod next ((iter iterator))
  (with-accessors ((index index)
                   (element initial-element)
                   (contents initial-contents)) iter
    (cond
      ((valid-p iter)
       (let ((current
              (cond
                (contents (funcall (accessor iter) contents index))
                (element element)
                (T index))))
         (incf index (increment iter))
         (funcall (id iter) current)))
      ((cyclic-p iter) (progn
                         (reset iter)
                         (next iter)))
      (T (progn
           (setf (exhausted-p iter) T)
           (when (not (quiet-p iter))
             (error 'stop-iteration-exception
                    :text (to-string iter))))))))

(defmethod take (n (iter iterator))
  (let ((values ()))
    (handler-case
        (dotimes (i n)
          (push (next iter) values))
      (stop-iteration-exception ()))
    (nreverse values)))

(defmethod take-while (pred (iter iterator))
  (let ((values ()))
    (handler-case
        (loop for value = (next iter)
           while (funcall pred value)
           do (push value values))
      (stop-iteration-exception ()))
    (nreverse values)))

(defmethod zip ((a iterator) (b iterator))
  (let ((vals ()))
    (handler-case 
        (loop for value-a = (next a)
           for value-b = (next b)
           do (push (cons value-a value-b) vals))
      (stop-iteration-exception ()))
    (nreverse vals)))

(defmacro make-iterator (&key (start 0) (end *limit*) (inc 1)
                           (id #'identity) (cyclic nil) (from-end nil)
                           (initial-contents ()) (initial-element nil))
  `(make-instance 'iterator :start (or ,start 0)
                  :end (or ,end *limit*) :inc ,inc :id ,id
                  :cyclic ,cyclic :from-end ,from-end
                  :initial-contents ,initial-contents
                  :initial-element ,initial-element))

(defmacro with-iterator ((name &key (start 0) (end *limit*) (increment 1)
                               (initial-contents nil) (id #'identity)) &body body)
  `(let ((,name (make-iterator :start ,start :end ,end
                               :inc ,increment :id ,id
                               :initial-contents ,initial-contents)))
     ,@body))

(defun get-varlist (all-clauses)
  (let ((varlist ())
        (endlist ()))
    (loop for (var tmp result) in all-clauses
       for iter = (if (listp tmp) (eval tmp) tmp)
       do (progn
            (push `(,var (next ,iter) (or (next ,iter) ,result)) varlist)
            (push `(exhausted-p ,iter) endlist)))
    (list varlist endlist)))

(defun list-to-alist (lst)
  (labels ((rec (lst alst)
             (cond 
               ((null lst) alst)
               ((null (cdr lst)) (error "odd number of args"))
               (T (rec (cddr lst) (cons (cons (first lst) (second lst)) alst))))))
    (nreverse (rec lst '()))))

(defun resolve-key-args (clause keys)
  (destructuring-bind ((var . sequence) &rest others) (list-to-alist clause)
    (append (list var sequence) 
            (loop for key in keys
               for value = (cdr (assoc key others))
               appending (list key value)))))

(defmacro do-sequence-iterators (((var iter &optional result) &rest more-clauses) &body body)
  (let ((all-clauses (cons (list var iter result) more-clauses)))
    `(handler-case
         (do ,@(get-varlist all-clauses)
             ,@body)
       (stop-iteration-exception()))))

(defmacro do-sequence-iterators* (((var iter &optional result) &rest more-clauses) &body body)
  (let ((all-clauses (cons (list var iter result) more-clauses)))
    `(handler-case
         (do* ,@(get-varlist all-clauses)
              ,@body)
       (stop-iteration-exception()))))

(defmacro do-sequence-iterator ((var iter &optional result) &body body)
  `(do-sequence-iterators* ((,var ,iter ,result))
     ,@body))

(defmacro dosequences* (((var sequence &key result start end from-end)
                         &rest more-clauses) &body body)
  (let* ((all-args (cons (list var sequence :start start :end end
                               :from-end from-end :result result) more-clauses))
         (key-args '(:start :end :from-end :result))
         (tmp-clauses (mapcar #'(lambda(x) (resolve-key-args x key-args)) all-args))
         (all-clauses (loop for (var sequence start end from-end result) in tmp-clauses
                         collect `(,var (make-iterator :start ,start :initial-contents ,sequence
                                                       :end ,end :from-end ,from-end) ,result))))
    `(do-sequence-iterators* ,all-clauses
       ,@body)))

(defmacro dosequence ((var sequence &key result start end from-end) &body body)
  `(dosequences* ((,var ,sequence :result ,result :start ,start :end ,end :from-end ,from-end))
     ,@body))
