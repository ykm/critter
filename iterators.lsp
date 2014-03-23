(defpackage :cl-iterators
  (:use :common-lisp)
  (:export :iterator
           :next
           :take
           :take-while
           :zip
           :reset
           :cyclic-p
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
             :documentation "Function to access to individual members of a initial-contents, #'nth for lists, #'elt for others")))

(defmethod reset ((iter iterator))
  (with-accessors ((start start) (index index)) iter
    (setf index start)))

(defmethod valid-p ((iter iterator))
  (with-accessors ((index index)
                   (check bound-check)
                   (end end)) iter
    (or (eq end *limit*) (funcall check index end))))

(defmethod next ((iter iterator))
  (with-accessors ((index index)
                   (element initial-element)
                   (contents initial-contents)) iter
    (cond
      ((valid-p iter)
       (let ((current
              (funcall (id iter)
                       (cond
                         ((not (null contents))
                          (funcall (accessor iter) contents index))
                         ((not (null element)) element)
                         (T index)))))
         (incf index (increment iter))
         current))
      ((cyclic-p iter) (progn
                         (reset iter)
                         (next iter)))
      ((not (quiet-p iter))
       (error 'stop-iteration-exception
              :text (to-string iter))))))

(defmethod take (n (iter iterator))
  (let ((has-ended nil))
    (loop for i from 1 to n
       for value = (handler-case
                       (next iter)
                     (stop-iteration-exception ()
                       (setf has-ended T)))
       while (not has-ended)
       collect value)))

(defmethod take-while (pred (iter iterator))
  (let ((has-ended nil))
    (loop for value = (handler-case
                          (next iter)
                        (stop-iteration-exception ()
                          (setf has-ended T)))
       while (and (not has-ended)
                  (funcall pred value))
       collect value)))

(defmethod zip ((a iterator) (b iterator))
  (let ((has-ended nil))
    (loop
       for value-a = (handler-case
                         (next a)
                       (stop-iteration-exception ()
                         (setf has-ended T)))
       for value-b = (handler-case
                         (next b)
                       (stop-iteration-exception ()
                         (setf has-ended T)))
       while (not has-ended)
       collect (cons value-a value-b))))

(defmacro make-iterator (&key (start 0) (end *limit*) (inc 1)
                           (id #'identity) (cyclic nil) (from-end nil)
                           (initial-contents ()) (initial-element nil))
  (let ((iter (gensym "iterator")))
    `(let ((,iter
            (make-instance 'iterator :start (or ,start 0)
                           :end (let ((len (1- (length ,initial-contents)))
                                      (end (or ,end *limit*)))
                                  (or (when (not (null ,initial-contents))
                                        (if (= end *limit*)
                                            len
                                            (when (> end len)
                                              (error "length out of bounds"))))
                                      end))
                           :inc ,inc :id ,id :cyclic ,cyclic
                           :initial-contents ,initial-contents
                           :initial-element ,initial-element)))
       (when (listp (initial-contents ,iter))
         (setf (accessor ,iter) #'(lambda(seq n) (nth n seq))))
       (with-accessors ((start start)
                        (end end)
                        (index index)
                        (increment increment)
                        (check bound-check)) ,iter
         (if ,from-end
             (progn
               (rotatef start end)
               (setf index start
                     check #'>=
                     increment (* -1 ,inc)))
             (setf index start)))
       ,iter)))

(defmacro with-iterator ((name &key (start 0) (end *limit*) (increment 1)
                               (initial-contents nil) (id #'identity)) &body body)
  `(let ((,name (make-iterator :start ,start :end ,end
                               :inc ,increment :id ,id
                               :initial-contents ,initial-contents)))
     ,@body))

(defmethod to-string ((iter iterator))
  (with-slots (start end increment id quiet contents cyclic) iter
    (format nil "start: ~A, end: ~A, quiet: ~A, inc: ~A, cyclic: ~A, initial-contents: ~A ~%"
            start end quiet increment cyclic contents)))

(defun get-sequence-iter-varlist (all-clauses)
  (list
   (loop for clause in all-clauses
      for var-name = (first clause)
      for tmp = (second clause)
      for iter-tmp = (if (listp tmp) (eval tmp) tmp)
      for result-tmp = (nth 2 clause)
      collect `(,var-name (next ,iter-tmp) (or (next ,iter-tmp) ,result-tmp)))
   `((some #'null (list ,@(loop for clause in all-clauses collect (car clause)))))))

(defun list-to-alist (lst)
  (labels ((rec (lst alst)
             (cond 
               ((null lst) alst)
               ((null (cdr lst)) (error "odd number of args"))
               (T (rec (cddr lst) (cons (cons (first lst) (second lst)) alst))))))
    (nreverse (rec lst '()))))

(defun resolve-key-args(clause key-args &optional (key-name-p T))
  (let* ((pairs (list-to-alist clause))
         (args (car pairs))
         (others (cdr pairs))
         (get #'(lambda(key) (cdr (assoc key others)))))
    (append (list (car args) (cdr args)) 
            (loop for key in key-args
               for value = (funcall get key) 
               appending (if key-name-p (list key value) (list value))))))

(defmacro do-sequence-iterators (((var iter &optional result) &rest more-clauses) &body body)
  (let ((all-clauses (cons (list var iter result) more-clauses)))
    `(do ,@(get-sequence-iter-varlist all-clauses)
         ,@body)))

(defmacro do-sequence-iterators* (((var iter &optional result) &rest more-clauses) &body body)
  (let ((all-clauses (cons (list var iter result) more-clauses)))
    `(do* ,@(get-sequence-iter-varlist all-clauses)
          ,@body)))

(defmacro do-sequence-iterator ((var iter &optional result) &body body)
  `(do-sequence-iterators* (,var ,iter ,result)
     ,@body))

(defmacro dosequences* (((var sequence &key result start end from-end)
                         &rest more-clauses) &body body)
  (let* ((all-args (cons (list var sequence :start start :end end
                               :from-end from-end :result result) more-clauses))
         (key-args '(:start :end :from-end :result))
         (tmp-clauses (mapcar #'(lambda(x) (resolve-key-args x key-args nil)) all-args))
         (all-clauses (loop for (var sequence start end from-end result) in tmp-clauses
                         collect `(,var (make-iterator :start ,start :initial-contents ,sequence
                                                       :end ,end :from-end ,from-end) ,result))))
    `(do-sequence-iterators* ,all-clauses
       ,@body)))

(defmacro dosequence ((var sequence &key result start end from-end) &body body)
  `(dosequences* ((,var ,sequence :result ,result :start ,start :end ,end :from-end ,from-end))
     ,@body))
