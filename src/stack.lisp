;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

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

(defgeneric stack-rows-copy (source destination element-type start-row)
  (:documentation "Method used to implement the copying of objects in STACK-ROW*, by copying the elements of SOURCE to DESTINATION, starting with the row index START-ROW in the latter.  Elements are coerced to ELEMENT-TYPE.

This method is only called when (DIMS SOURCE) was non-nil.  It is assumed that it onlychanges elements in DESTINATION which are supposed to be copies of SOURCE.  DESTINATION is always a matrix with element-type upgraded from ELEMENT-TYPE, and its NCOL should match the relevant dimension of SOURCE.

All objects have a fallback method, defined using AS-ARRAY.  The only reason for definining a method is efficiency.")
  (:method (source destination element-type start-row)
    (stack-rows-copy (as-array source) destination element-type start-row))
  (:method ((source array) destination element-type start-row)
    (copy-row-major-block source destination element-type
                          :destination-start (* start-row (ncol destination)))))

(defun stack-rows* (element-type &rest objects)
  "Stack OBJECTS row-wise into an array of the given ELEMENT-TYPE, coercing if necessary.  Always return a simple array of rank 2.

How objects are used depends on their dimensions, queried by DIMS:

- when the object has 0 dimensions, fill a row with the element.

- when the object has 1 dimension, use it as a row.

- when the object has 2 dimensions, use it as a matrix.

When applicable, compatibility of dimensions is checked, and the result is used to determine the number of columns.  When all objects have 0 dimensions, the result has one column."
  (let (ncol)
    (flet ((check-ncol (dim)
             (if ncol
                 (assert (= ncol dim))
                 (setf ncol dim))))
      (let* ((nrow 0)
             (start-rows-and-dims (mapcar
                                   (lambda (object)
                                     (let* ((dims (dims object))
                                            (increment (ematch dims
                                                         (nil 1)
                                                         ((list d0) (check-ncol d0)
                                                          1)
                                                         ((list d0 d1) (check-ncol d1)
                                                          d0))))
                                       (prog1 (cons nrow dims)
                                         (incf nrow increment))))
                                   objects))
             (ncol (anaphora:aif ncol anaphora:it 1)))
        (anaphora:aprog1 (make-array (list nrow ncol) :element-type element-type)
          (mapc (lambda (start-rows-and-dims object)
                  (destructuring-bind (start-row &rest dims)
                      start-rows-and-dims
                    (if dims
                        (stack-rows-copy object anaphora:it element-type start-row)
                        (fill (displace anaphora:it ncol (* start-row ncol))
                              (coerce object element-type)))))
                start-rows-and-dims objects))))))

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
    (ematch (dims source)
      ((list _)
       (loop for row below (nrow destination)
             do (setf (aref destination row start-col)
                      (coerce (aref source row) element-type))))
      ((list _ ncol)
       (loop for row below (nrow destination)
             for source-start by ncol
             do (copy-row-major-block source destination element-type
                                      :source-start source-start
                                      :source-end (+ source-start ncol)
                                      :destination-start (array-row-major-index
                                                          destination
                                                          row start-col)))))))

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
                                            (increment (ematch dims
                                                         (nil 1)
                                                         ((list d0) (check-nrow d0)
                                                          1)
                                                         ((list d0 d1) (check-nrow d0)
                                                          d1))))
                                       (prog1 (cons ncol dims)
                                         (incf ncol increment))))
                                   objects))
             (nrow (anaphora:aif nrow anaphora:it 1)))
        (anaphora:aprog1 (make-array (list nrow ncol) :element-type element-type)
          (mapc (lambda (start-cols-and-dims object)
                  (destructuring-bind (start-col &rest dims)
                      start-cols-and-dims
                    (if dims
                        (stack-cols-copy object anaphora:it element-type start-col)
                        (loop for row below nrow
                              with object = (coerce object element-type)
                              do (setf (aref anaphora:it row start-col) object)))))
                start-cols-and-dims objects))))))

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
    (anaphora:aprog1 (make-array (cons sum-first dim-rest) :element-type element-type)
      (loop with cumulative-sum = 0
            for array in arrays
            do (let* ((dim-first (array-dimension array 0))
                      (end (+ cumulative-sum dim-first)))
                 (setf (partition anaphora:it cumulative-sum end) array
                       cumulative-sum end))))))

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
