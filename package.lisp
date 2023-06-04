(defpackage cliter
  (:use #:cl #:alexandria)
  (:shadow
   #:length
   #:nth
   #:map
   #:mapc
   #:mapcar
   #:mapcan
   #:remove-if-not
   #:remove-if
   #:remove
   #:position-if
   #:position-if-not
   #:position
   #:find-if
   #:find-if-not
   #:find
   #:count-if
   #:count-if-not
   #:count
   #:append
   #:reduce
   #:remove-duplicates
   #:subseq
   #:first
   #:second
   #:third
   #:fourth
   #:fifth
   #:sixth
   #:seventh
   #:eighth
   #:ninth
   #:tenth
   #:lastcar)
  (:export
   #:iterator
   #:function-iterator
   #:next
   #:doiter
   #:list-iterator
   #:vector-iterator
   #:iterator-list
   #:iterator-vector
   #:map
   #:mapc
   #:mapcar
   #:flatmap
   #:mapcan
   #:remove-if-not
   #:remove-if
   #:remove
   #:position-if
   #:position-if-not
   #:position
   #:find-if
   #:find-if-not
   #:find
   #:count-if
   #:count-if-not
   #:count
   #:append
   #:reduce
   #:remove-duplicates
   #:subseq
   #:drop-until-take-while
   #:take
   #:drop
   #:drop-until
   #:drop-while
   #:take-while
   #:take-until
   #:length
   #:nth
   #:first
   #:second
   #:third
   #:fourth
   #:fifth
   #:sixth
   #:seventh
   #:eighth
   #:ninth
   #:tenth
   #:lastcar))

(in-package #:cliter)

(deftype iterator () '(function () (values t t)))

(declaim (ftype (function (function) (values iterator)) function-iterator)
         (ftype (function (iterator) (values t t)) next))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'function-iterator) #'identity
        (fdefinition 'next) #'funcall))

(defmacro doiter ((var iterator &optional (result 'nil)) &body body)
  (with-gensyms (iter present-p)
    `(loop :with ,iter := ,iterator
           :with ,var :and ,present-p
           :do (setf (values ,var ,present-p) (next ,iter))
           :while ,present-p
           :do (progn ,@body)
           :finally (return ,result))))

(declaim (ftype (function (list) iterator) list-iterator))
(defun list-iterator (list)
  (declare (type list list))
  (setf list (cons nil list))
  (function-iterator
   (lambda ()
    (setf list (cdr list))
    (values (car list) list))))

(declaim (ftype (function (vector &key (:start fixnum) (:end fixnum) (:from-end boolean)) iterator) vector-iterator))
(defun vector-iterator (vector &key (start 0) (end (cl:length vector)) (from-end nil))
  (declare (type fixnum start end))
  (function-iterator
   (if from-end
       (lambda ()
         (if (< start end)
             (values (aref vector (decf end)) t)
             (values nil nil)))
       (lambda ()
         (if (< start end)
             (values (prog1 (aref vector start) (incf start)) t)
             (values nil nil))))))

(declaim (ftype (function (iterator) list) iterator-list))
(defun iterator-list (iterator)
  (let ((list nil))
    (doiter (value iterator (nreverse list))
      (push value list))))

(declaim (ftype (function (iterator) vector) iterator-vector))
(defun iterator-vector (iterator)
  (let ((vector (make-array 0 :fill-pointer 0 :adjustable t)))
    (doiter (value iterator vector)
      (vector-push-extend value vector))))

(declaim (ftype (function (function iterator &rest iterator) (values iterator)) map))
(defun map (function iterator &rest iterators)
  (function-iterator
   (lambda ()
    (loop :with value :and present-p
          :for iter :in (cons iterator iterators)
          :do (setf (values value present-p) (next iter))
          :unless present-p
            :return (values nil nil)
          :collect value :into args
          :finally (return (values (apply function args) t))))))

(declaim (ftype (function (function &rest iterator) iterator) flatmap))
(defun flatmap (function &rest iterators)
  (let ((iterator nil))
    (function-iterator
     (lambda ()
      (loop :do (when iterator
                  (multiple-value-bind (value present-p) (next iterator)
                    (when present-p
                      (return (values value present-p)))))
            :while (loop :with value :and present-p
                         :for of-iterator :in iterators
                         :do (setf (values value present-p) (next of-iterator))
                         :always present-p
                         :collect value :into args
                         :finally (setf iterator (apply function args)))
            :finally (return (values nil nil)))))))

(declaim (ftype (function (function iterator &rest iterator) (values null)) mapc))
(defun mapc (function iterator &rest iterators)
  (progn (apply #'map function iterator iterators) nil))

(setf (fdefinition 'mapcar) (fdefinition 'map)
      (fdefinition 'mapcan) (fdefinition 'flatmap))

(declaim (ftype (function (&rest iterator) (values iterator)) append))
(defun append (&rest iterators)
  (flatmap #'identity (list-iterator iterators)))

(declaim (ftype (function (function iterator &key (:key function) (:initial-value t)) (values t)) reduce))
(defun reduce (function iterator &key (key #'identity) (initial-value nil initial-value-provided-p))
  (loop :with value :and present-p
        :for acc := (if initial-value-provided-p initial-value
                        (multiple-value-bind (value present-p) (next iterator)
                          (unless present-p
                            (return (funcall function)))
                          (values (funcall key value))))
          :then (funcall function acc value)
        :do (setf (values value present-p) (next iterator))
        :while present-p
        :do (setf value (funcall key value))
        :finally (return acc)))

(declaim (ftype (function (function iterator) (values iterator)) remove-if-not))
(defun remove-if-not (predicate iterator)
  (function-iterator
   (lambda ()
    (loop :with value :and present-p
          :do (setf (values value present-p) (next iterator))
          :while present-p :until (funcall predicate value)
          :finally (return (values value present-p))))))

(declaim (ftype (function (function iterator) (values iterator)) remove-if))
(defun remove-if (predicate iterator)
  (remove-if-not (complement predicate) iterator))

(declaim (ftype (function (t iterator &key (:key function) (:test function)) (values iterator)) remove))
(defun remove (item iterator &key (key #'identity) (test #'eql))
  (remove-if (lambda (value) (funcall test item (funcall key value))) iterator))

(declaim (ftype (function (function iterator) (values t)) position-if))
(defun position-if (predicate iterator)
  (loop :with value :and present-p
        :for index :of-type fixnum :from 0
        :do (setf (values value present-p) (next iterator))
        :while present-p
        :when (funcall predicate value)
          :return index))

(declaim (ftype (function (function iterator) (values t)) position-if-not))
(defun position-if-not (predicate iterator)
  (position-if (complement predicate) iterator))

(declaim (ftype (function (t iterator &key (:key function) (:test function)) (values t)) position))
(defun position (item iterator &key (key #'identity) (test #'eql))
  (position-if (lambda (value) (funcall test item (funcall key value))) iterator))

(declaim (ftype (function (function iterator) (values t)) find-if))
(defun find-if (predicate iterator)
  (loop :with value :and present-p
        :do (setf (values value present-p) (next iterator))
        :while present-p
        :when (funcall predicate value)
          :return value))

(declaim (ftype (function (function iterator) (values t)) find-if-not))
(defun find-if-not (predicate iterator)
  (find-if (complement predicate) iterator))

(declaim (ftype (function (t iterator &key (:key function) (:test function)) (values t)) find))
(defun find (item iterator &key (key #'identity) (test #'eql))
  (find-if (lambda (value) (funcall test item (funcall key value))) iterator))

(declaim (ftype (function (function iterator) (values non-negative-fixnum)) count-if))
(defun count-if (predicate iterator)
  (loop :with value :and present-p
        :do (setf (values value present-p) (next iterator))
        :while present-p
        :counting (funcall predicate value)))

(declaim (ftype (function (function iterator) (values non-negative-fixnum)) count-if-not))
(defun count-if-not (predicate iterator)
  (count-if (complement predicate) iterator))

(declaim (ftype (function (iterator) (values non-negative-fixnum)) length))
(defun length (iterator)
  (count-if (constantly t) iterator))

(declaim (ftype (function (non-negative-fixnum iterator) (values t)) nth))
(defun nth (n iterator &aux (i 0))
  (declare (type non-negative-fixnum i))
  (doiter (elem iterator)
    (when (= i n) (return-from nth elem))
    (incf i)))

(declaim (ftype (function (iterator) (values t)) first)
         (inline first))
(defun first (iterator)
  (nth 0 iterator))

(declaim (ftype (function (iterator) (values t)) second)
         (inline second))
(defun second (iterator)
  (nth 1 iterator))

(declaim (ftype (function (iterator) (values t)) third)
         (inline third))
(defun third (iterator)
  (nth 2 iterator))

(declaim (ftype (function (iterator) (values t)) fourth)
         (inline fourth))
(defun fourth (iterator)
  (nth 3 iterator))

(declaim (ftype (function (iterator) (values t)) fifth)
         (inline fifth))
(defun fifth (iterator)
  (nth 4 iterator))

(declaim (ftype (function (iterator) (values t)) sixth)
         (inline sixth))
(defun sixth (iterator)
  (nth 5 iterator))

(declaim (ftype (function (iterator) (values t)) seventh)
         (inline seventh))
(defun seventh (iterator)
  (nth 6 iterator))

(declaim (ftype (function (iterator) (values t)) eighth)
         (inline eighth))
(defun eighth (iterator)
  (nth 7 iterator))

(declaim (ftype (function (iterator) (values t)) ninth)
         (inline ninth))
(defun ninth (iterator)
  (nth 8 iterator))

(declaim (ftype (function (iterator) (values t)) tenth)
         (inline tenth))
(defun tenth (iterator)
  (nth 9 iterator))

(declaim (ftype (function (iterator) (values t)) lastcar))
(defun lastcar (iterator &aux last)
  (doiter (elem iterator last)
    (setf last elem)))

(declaim (ftype (function (t iterator &key (:key function) (:test function)) (values integer)) count))
(defun count (item iterator &key (key #'identity) (test #'eql))
  (count-if (lambda (value) (funcall test item (funcall key value))) iterator))

(declaim (ftype (function (iterator &key (:key function) (:test function)) (values iterator)) remove-duplicates))
(defun remove-duplicates (iterator &key (key #'identity) (test #'eql))
  (let ((hash-table (make-hash-table :test test)))
    (remove-if (lambda (value)
                 (setf value (funcall key value))
                 (prog1 (gethash value hash-table)
                   (setf (gethash value hash-table) t)))
               iterator)))

(declaim (ftype (function (iterator non-negative-fixnum &optional non-negative-fixnum) (values iterator)) subseq))
(defun subseq (iterator start &optional end)
  (let ((i 0))
    (declare (type non-negative-fixnum i))
    (function-iterator
     (lambda ()
      (loop :until (>= i start)
            :while (nth-value 1 (next iterator))
            :do (incf i))
      (if (or (not end) (<= (incf i) end))
          (next iterator)
          (values nil nil))))))

(declaim (ftype (function (iterator non-negative-fixnum) (values iterator)) take))
(defun take (iterator n)
  (subseq iterator 0 n))

(declaim (ftype (function (iterator non-negative-fixnum) (values iterator)) drop))
(defun drop (iterator n)
  (subseq iterator n))

(declaim (ftype (function (function function iterator) (values iterator)) drop-until-take-while))
(defun drop-until-take-while (drop-predicate take-predicate iterator)
  (let ((taking-p t)
        (after-drop-p nil))
    (function-iterator
     (lambda ()
      (if taking-p
          (multiple-value-bind (value present-p) (next iterator)
            (unless after-drop-p
              (loop :until (funcall drop-predicate value)
                    :do (setf (values value present-p) (next iterator))
                    :finally (setf after-drop-p t)))
            (if (funcall take-predicate value)
                (values value present-p)
                (values nil (setf taking-p nil))))
          (values nil nil))))))

(declaim (ftype (function (function iterator) (values iterator)) drop-until))
(defun drop-until (predicate iterator)
  (drop-until-take-while predicate (constantly t) iterator))

(declaim (ftype (function (function iterator) (values iterator)) drop-while))
(defun drop-while (predicate iterator)
  (drop-until (complement predicate) iterator))

(declaim (ftype (function (function iterator) (values iterator)) take-while))
(defun take-while (predicate iterator)
  (drop-until-take-while (constantly t) predicate iterator))

(declaim (ftype (function (function iterator) (values iterator)) take-until))
(defun take-until (predicate iterator)
  (take-while (complement predicate) iterator))
