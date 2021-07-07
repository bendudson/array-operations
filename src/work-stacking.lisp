;;;; Scratch work for 'better' stacking functions.
;;;; By 'better', I mean more readable, and maybe more performant.

;; TODO: Think about organization and naming.
;; Some of the existing code seems to assume a domain of matrices, vectors, etc.

;; TODO: Generalize, or expand, some of these to tensors of rank > 2?

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
