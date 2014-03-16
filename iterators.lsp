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
           :do-sequence-iterator
           :do-sequence-iterators
           :do-sequence-iterators*
           :dosequences*
           :dosequence
           :list-to-alist))

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
           :documentation "Whether the iteration is cyclic, in case where initial-contents are supplied the sequence re-iterated otherwise the iteration resets the index to start")
   (initial-contents :initform nil :initarg :initial-contents
                     :documentation "A list/sequence of initial-contents to be iterated")
   (initial-element :initform nil :initarg :initial-element
                    :documentation "When supplied and no initial-contents supplied, iteration returns the value instead of the index")
   (bound-check :initform #'< :initarg :from-end
                :documentation "When T, direction of iteration is from-end")
   (accessor :initform #'nth
             :documentation "Function to access to individual members of a initial-contents, #'nth for lists, #'elt for others")))

(defmethod reset ((iter iterator))
  (with-slots (start index) iter
    (setf index start)))

(defmethod next ((iter iterator))
  (with-slots (index end increment id quiet cyclic initial-element
                     accessor bound-check initial-contents) iter
    (if (or (funcall bound-check index end) (eq end -1))
        (let ((current
               (funcall id (cond
                             ((not (null initial-contents))
                              (funcall accessor index initial-contents))
                             ((not (null initial-element)) initial-element)
                             (T index)))))
          (incf index increment)
          current)
        (if cyclic
            (progn
              (reset iter)
              (next iter))
            (when (not quiet)
              (error "Stop iteration, iterations have exhausted."))))))

(defmethod take (n (iter iterator))
  (loop for i from 1 to n
     for j = (next iter) 
     while j
     collect j))

(defmethod take-while (pred (iter iterator))
  (loop for i = (next iter)
     while (and i (funcall pred i))
     collect i))

(defmethod take-until (pred (iter iterator))
  (loop for i = (next iter)
     until (or (not i) (funcall pred i))
     collect i))

(defmethod zip ((a iterator) (b iterator))
  (loop for i = (next a)
     for j = (next b) 
     while (and i j)
     collect (cons i j)))

(defmacro make-iterator (&key (start 0) (end -1) (inc 1) (id #'identity) (cyclic nil)
                           (initial-contents ()) (from-end nil) (initial-element nil))
  (let ((iter (gensym "iterator")))
    `(let ((,iter (make-instance 'iterator :start (or ,start 0)
                                 :end (or (when (not (null ,initial-contents))
                                            (let ((len (length ,initial-contents)))
                                              (if (= ,end -1)
                                                  len
                                                  (when (>= ,end len)
                                                    (error "length out of bounds")))))
                                          ,end)
                                 :inc ,inc :id ,id :cyclic ,cyclic
                                 :initial-contents ,initial-contents
                                 :initial-element ,initial-element)))
       (with-slots (start end index initial-contents increment accessor bound-check) ,iter
         (when (not (listp initial-contents))
           (setf accessor #'(lambda(n seq) (elt seq n))))
         (if ,from-end
             (progn
               (rotatef start end)
               (setf index start
                     bound-check #'>
                     increment (* -1 ,inc)))
             (setf index start)))
       ,iter)))

(defmacro with-iterator ((name &key (start 0) (end -1) (increment 1)
                               (initial-contents nil) (id #'identity)) &body body)
  `(let ((,name (make-iterator :start ,start :end ,end
                               :inc ,increment :id ,id
                               :initial-contents ,initial-contents)))
     ,@body))

(defmethod to-string ((iter iterator))
  (with-slots (start end increment id quiet initial-contents cyclic) iter
    (format T "start: ~A, end: ~A, quiet: ~A, inc: ~A, cyclic: ~A, initial-contents: ~A ~%"
            start end quiet increment cyclic initial-contents)))

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
