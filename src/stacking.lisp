;;;; Functions for composing arrays into new arrays, by "stacking".
;;;;
;;;; One may think of stacking blocks as the guiding metaphor.
;;;;
;;;; For example, stack two row vectors to yield a 2x2 matrix:
;;;;
;;;; (stack-rows #(1 2) #(3 4)) -> #2A((1 2)
;;;;                                   (3 4))

(defpackage :array-operations/stacking
  (:use :cl :array-operations/generic
            :array-operations/utilities
            :array-operations/displacing)
  (:import-from :array-operations/transforming
                :complete-permutation
                :invert-permutation
                :permute)
  (:import-from :alexandria
                :curry)
  (:export :copy-row-major-block
           :stack-rows* :stack-rows
           :stack-cols-copy :stack-cols* :stack-cols
           :stack* :stack))

(in-package :array-operations/stacking)

(defun copy-row-major-block (source-array destination-array element-type
                             &key (source-start 0)
                                  (source-end (size source-array))
                                  (destination-start 0))
  "Copy elements with row major indexes between the given start and end from SOURCE to DESTINATION, respectively.  Elements are coerced to ELEMENT-TYPE when necessary.  Return no values.

This function should be used to implement copying of contiguous row-major blocks of elements, most optimizations should happen here."
  (let ((count (- source-end source-start)))
    (let ((source (displace source-array count source-start))
          (destination (displace destination-array count destination-start)))
      (if (subtypep (element-type source-array) element-type)
          (replace destination source)
          (map-into destination (lambda (element) (coerce element element-type))
                    source))))
  (values))

;; TODO: Are these the best names for this and 'cols', following)?
(defun rows (object)
  "Return the number of rows of an array or scalar. Scalars return 1."
  (etypecase object
    (array (ecase (rank object)
             (1 1)
             (2 (dim object 0))))
    (number 1)))

;; TODO: Are these the best names for this and 'rows', previous)?
(defun cols (object)
  "Return the number of columns of an array or scalar. Scalars return 1."
  (etypecase object
    (array (ecase (rank object)
             (1 (dim object 0))
             (2 (dim object 1))))
    (number 1)))

(defun stack-rows* (element-type &rest objects)
  "Stack OBJECTS, row-wise, into a simple 2D array of given ELEMENT-TYPE.

   Objects may be arrays or scalars.
   Array arguments must all have the same number of columns.
   Scalars are repeated to fill their row in the result."
  (let* ((array-objects (remove-if-not #'arrayp objects))
         (array-objects-cols (mapcar #'cols array-objects)))
    (when array-objects
      (assert (apply #'= array-objects-cols) nil
              "All ARRAY-type arguments must have the same width"))
    (let* ((height (reduce #'+ (mapcar #'rows objects)))
           (width (or (first array-objects-cols) 1)) ; 1 when only scalars.
           (result (make-array (list height width)
                               :element-type element-type))
           (flat-result (flatten result)))
      (loop as object in objects
            with cursor = 0
            when (arrayp object)
              ;; If we didn't need to coerce, we could just use REPLACE.
              do (loop for object-element across (flatten object)
                       for idx from cursor
                       do (setf (aref flat-result idx)
                                (coerce object-element element-type)))
                 (incf cursor (array-total-size object))
            else
              do (fill flat-result
                       (coerce object element-type)
                       :start cursor
                       :end (+ cursor width))
                 (incf cursor width))
      result)))

(defun stack-rows (&rest objects)
  "Like STACK-ROWS*, with ELEMENT-TYPE T."
  (apply #'stack-rows* t objects))

(defgeneric stack-cols-copy (source destination element-type start-col)
  (:documentation "Method used to implement the copying of objects in STACK-COL*, by copying the elements of SOURCE to DESTINATION, starting with the column index START-COL in the latter.  Elements are coerced to ELEMENT-TYPE.

This method is only called when (DIMS SOURCE) was non-nil.  It is assumed that it only changes elements in DESTINATION which are supposed to be copies of SOURCE.  DESTINATION is always a matrix with element-type upgraded from ELEMENT-TYPE, and its NROW should match the relevant dimension of SOURCE.

All objects have a fallback method, defined using AS-ARRAY.  The only reason for definining a method is efficiency.")
  (:method (source destination element-type start-col)
    (stack-cols-copy (as-array source) destination element-type start-col))
  (:method ((source array) destination element-type start-col)
    (let ((dims (dims source)))
      (ecase (length dims)
        (1 (loop for row below (nrow destination)
                 do (setf (aref destination row start-col)
                          (coerce (aref source row) element-type))))
        (2 (let ((ncol (second dims)))
             (loop for row below (nrow destination)
                   for source-start by (second (dims source))
                   do (copy-row-major-block source destination element-type
                                            :source-start source-start
                                            :source-end (+ source-start (second dims))
                                            :destination-start (array-row-major-index
                                                                destination
                                                                row start-col)))))))))

(defun stack-cols* (element-type &rest objects)
  "Stack OBJECTS column-wise into an array of the given ELEMENT-TYPE, coercing if necessary.  Always return a simple array of rank 2.

How objects are used depends on their dimensions, queried by DIMS:

- when the object has 0 dimensions, fill a column with the element.

- when the object has 1 dimension, use it as a column.

- when the object has 2 dimensions, use it as a matrix.

When applicable, compatibility of dimensions is checked, and the result is used to determine the number of rows.  When all objects have 0 dimensions, the result has one row."
  (let (nrow)
    (flet ((check-nrow (dim)
             (if nrow
                 (assert (= nrow dim))
                 (setf nrow dim))))
      (let* ((ncol 0)
             (start-cols-and-dims (mapcar
                                   (lambda (object)
                                     (let* ((dims (dims object))
                                            (increment (ecase (length dims)
                                                         (0 1)
                                                         (1 (check-nrow (first dims))
                                                            1)
                                                         (2 (check-nrow (first dims))
                                                            (second dims)))))
                                       (prog1 (cons ncol dims)
                                         (incf ncol increment))))
                                   objects))
             (nrow (or nrow 1)))
        (let ((result (make-array (list nrow ncol) :element-type element-type)))
          (mapc (lambda (start-cols-and-dims object)
                  (destructuring-bind (start-col &rest dims)
                      start-cols-and-dims
                    (if dims
                        (stack-cols-copy object result element-type start-col)
                        (loop for row below nrow
                              with object = (coerce object element-type)
                              do (setf (aref result row start-col) object)))))
                start-cols-and-dims objects)
          result)))))

(defun stack-cols (&rest objects)
  "Like STACK-COLS*, with ELEMENT-TYPE T."
  (apply #'stack-cols* t objects))

(defun stack*0 (element-type arrays)
  "Stack arrays along the 0 axis, returning an array with given ELEMENT-TYPE."
  (let* ((array-first (car arrays))
         (dim-rest (cdr (array-dimensions array-first)))
         (sum-first
          (reduce #'+ arrays
                  :key (lambda (array)
                         (let ((dimensions (array-dimensions array)))
                           (unless (eq array array-first)
                             (assert (equal dim-rest (cdr dimensions)) ()
                                     "Array ~A has incomplatible dimensions"
                                     array))
                           (first dimensions))))))
    (let ((result (make-array (cons sum-first dim-rest) :element-type element-type)))
      (loop with cumulative-sum = 0
            for array in arrays
            do (let* ((dim-first (array-dimension array 0))
                      (end (+ cumulative-sum dim-first)))
                 (setf (partition result cumulative-sum end) array
                       cumulative-sum end)))
      result)))

(defun stack* (element-type axis array &rest arrays)
  "Stack array arguments along AXIS.  ELEMENT-TYPE determines the element-type
of the result."
  (if arrays
      (let ((all-arrays (cons array arrays)))
        (if (= axis 0)
            (stack*0 element-type all-arrays)
            (let ((permutation (complete-permutation axis (array-rank array))))
              ;; serious contender for the Least Efficient Implementation Award
              (permute (invert-permutation permutation)
                       (stack*0 element-type
                                (mapcar (curry #'permute permutation)
                                        all-arrays))))))
      array))

(defun stack (axis array &rest arrays)
  "Like STACK*, with element-type T."
  (apply #'stack* t axis array arrays))
