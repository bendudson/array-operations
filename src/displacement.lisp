;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; displacement and flattening

(defun displace (array dimensions &optional (offset 0))
  "Shorthand function for displacing an array."
  (make-array (ensure-dimensions dimensions)
              :displaced-to array
              :displaced-index-offset offset
              :element-type (array-element-type array)))

(defun flatten (array)
  "Return ARRAY flattened to a vector.  Will share structure."
  (displace array (array-total-size array)))

;;; subarrays

(defun split (array rank)
  "Return an array of subarrays, split off at RANK.  All subarrays are
displaced and share structure."
  (let ((array-rank (array-rank array)))
    (cond
      ((or (zerop rank) (= rank array-rank))
       array)
      ((< 0 rank array-rank)
       (let* ((dimensions (array-dimensions array))
              (result (make-array (subseq dimensions 0 rank)))
              (sub-dimensions (subseq dimensions rank))
              (sub-size (product sub-dimensions)))
         (dotimes (index (array-total-size result))
           (setf (row-major-aref result index)
                 (displace array sub-dimensions (* index sub-size))))
         result))
      (t (error "Rank ~A outside [0,~A]." rank array-rank)))))

(defun sub-location% (dimensions subscripts)
  "Return (values OFFSET REMAINING-DIMENSIONS) that can be used to displace a
row-major subarray starting at SUBSCRIPTS in an array with the given
DIMENSIONS.  NOT EXPORTED."
  (let* (rev-dimensions
         rev-subscripts
         (tail (do ((dimensions dimensions (cdr dimensions))
                    (subscripts subscripts (cdr subscripts)))
                   ((not subscripts) dimensions)
                 (assert dimensions ()
                         "More subscripts than dimensions.")
                 (let ((s (car subscripts))
                       (d (car dimensions)))
                   (declare (type fixnum d))
                   (assert (and (integerp s) (< -1 s d)) ()
                           "Invalid subscript.")
                   (push s rev-subscripts)
                   (push d rev-dimensions))))
         (product (product tail))
         (sum 0))
    (declare (type fixnum product sum))
    (mapc (lambda (d s)
            (declare (type fixnum d s))
            (incf sum (the fixnum (* product s)))
            (multf product d))
          rev-dimensions rev-subscripts)
    (values sum tail)))

(defun sub (array &rest subscripts)
  "Given a partial list of subscripts, return the subarray that starts there,
with all the other subscripts set to 0, dimensions inferred from the original.
If no subscripts are given, the original array is returned.  Implemented by
displacing, may share structure."
  (if subscripts
      (multiple-value-bind (offset dimensions)
          (sub-location% (array-dimensions array) subscripts)
        (if dimensions
            (displace array dimensions offset)
            (apply #'aref array subscripts)))
      array))

(defun copy-into (target source)
  "Copy SOURCE into TARGET, for array arguments of compatible
dimensions (checked).  Return TARGET, making the implementation of the
semantics of SETF easy."
  (assert (same-dimensions? target source))
  (replace (flatten target) (flatten source))
  target)

(defun (setf sub) (value array &rest subscripts)
  (multiple-value-bind (subarray atom?)
      (apply #'sub array subscripts)
    (if atom?
        (setf (apply #'aref array subscripts) value)
        (copy-into subarray value))))

(defun partition (array start &optional (end (array-dimension array 0)))
  "Return a subset of the array, on the first indexes between START and END."
  (let* ((d0 (array-dimension array 0))
         (stride (/ (array-total-size array) d0)))
    (assert (and (<= 0 start) (< start end) (<= end d0)))
    (displace array (cons (- end start) (cdr (array-dimensions array)))
              (* start stride))))

(defun (setf partition) (value array start
                         &optional (end (array-dimension array 0)))
  (copy-into (partition array start end) value))

(defun combine (array &optional element-type)
  "The opposite of SUBARRAYS.  If ELEMENT-TYPE is not given, it is inferred
from the first element of array, which also determines the dimensions.  If
that element is not an array, the original ARRAY is returned as it is."
  (unless (arrayp array)
    (return-from combine array))
  (let ((first (row-major-aref array 0)))
    (if (arrayp first)
        (let* ((dimensions (array-dimensions array))
               (sub-dimensions (array-dimensions first))
               (element-type (anaphora:aif element-type anaphora:it (array-element-type first)))
               (result (make-array (append dimensions sub-dimensions)
                                   :element-type element-type))
               (length (product dimensions))
               (displaced (displace result (cons length sub-dimensions))))
          (dotimes (index length)
            (setf (sub displaced index) (row-major-aref array index)))
          result)
        array)))

;;; subvector

(defun subvec (vector start &optional (end (length vector)))
  "Displaced vector between START and END."
  (displace vector (- end start) start))

(declaim (inline (setf subvec)))
(defun (setf subvec) (value vector start &optional (end (length vector)))
  ;; just a synonym for (setf subseq), except for checking the length
  (assert (length= value (- end start)))
  (setf (subseq vector start end) value))

;;; reshaping
(defun fill-in-dimensions (dimensions size)
  "If one of the dimensions is missing (indicated with T), replace it with a
dimension so that the total product equals SIZE.  If that's not possible,
signal an error.  If there are no missing dimensions, just check that the
product equals size.  Also accepts other dimension specifications (integer,
array)."
  (anaphora:aetypecase dimensions
    ((integer 0) (assert (= size anaphora:it)) (list anaphora:it))
    (array (assert (= size (size anaphora:it))) (dims anaphora:it))
    (list (flet ((missing? (dimension) (eq dimension t)))
            (let ((missing)
                  (product 1))
              (loop for dimension in dimensions
                    do (if (missing? dimension)
                           (progn
                             (assert (not missing) ()
                                     "More than one missing dimension.")
                             (setf missing t))
                           (progn
                             (check-type dimension (integer 1))
                             (multf product dimension))))
              (if missing
                  (multiple-value-bind (fraction remainder)
                      (floor size product)
                    (assert (zerop remainder) ()
                            "Substitution does not result in an integer ~ dimension.")
                    (mapcar (lambda (dimension)
                              (if (missing? dimension) fraction dimension))
                            dimensions))
                  (progn
                    (assert (= size product))
                    dimensions)))))))

(defun reshape (array dimensions &optional (offset 0))
  "Reshape ARRAY using DIMENSIONS (which can also be dimension
specifications).  If DIMENSIONS is a list, it may contain a single element T
which will be calculated to match the total size of the resulting array."
  (let* ((size (array-total-size array))
         (dimensions (fill-in-dimensions dimensions (- size offset))))
    (displace array dimensions offset)))

(defun reshape-col (array)
  "Array reshaped as an Nx1 matrix."
  (reshape array '(t 1)))

(defun reshape-row (array)
  "Array reshaped as an 1xN matrix."
  (reshape array '(1 t)))
