;;;; Functions for transforming arrays in various ways.

(defpackage :array-operations/transforming
  (:use :cl :array-operations/generic
            :array-operations/utilities
            :array-operations/displacing)
  (:import-from :alexandria
                :compose
                :curry
                :ensure-list
                :iota)
  (:export :coercing
           :each*      :each
           :margin*    :margin
           :outer*     :outer
           :vectorize* :vectorize :vectorize!
           :invert-permutation
           :complete-permutation
           :complement-permutation
           :identity-permutation-p
           :identity-permutation? ; deprecated alias for above
           :permutation-invalid-index
           :permutation-repeated-index
           :permutation-incompatible-rank
           :permute
           :recycle
           :across))

(in-package :array-operations/transforming)

;;; coercing can be used with * forms

(defun coercing (element-type &optional (function #'identity))
  "Return a function composed of a univariate function that coerces to ELEMENT-TYPE and function.  When FUNCTION is not given, return a closure that coerces to ELEMENT-TYPE."
  (compose (lambda (value) (coerce value element-type))
           function))


;;; permutations
;;;
;;; A permutation is a list of nonnegative, non-repeated integers, below some
;;; rank (of the array it is applied to).

(define-condition permutation-repeated-index (error)
  ((index :initarg :index)))

(define-condition permutation-invalid-index (error)
  ((index :initarg :index)))

(define-condition permutation-incompatible-rank (error)
  ())

(defun permutation-flags (permutation &optional (rank (length permutation)))
  "Make a bit vector of flags with indexes from PERMUTATION, signalling errors
for invalid and repeated indices.  NOT EXPORTED."
  (let ((result (make-array rank :element-type 'bit :initial-element 0)))
    (map nil (lambda (p)
               (assert (and (integerp p) (< -1 p rank)) ()
                       'permutation-invalid-index :index p)
               (assert (zerop (aref result p)) ()
                       'permutation-repeated-index :index p)
               (setf (aref result p) 1))
         permutation)
    result))

(defun check-permutation (permutation
                          &optional (rank (length permutation) rank-supplied-p))
  "Check if PERMUTATION is a valid permutation (of the given RANK), and signal
an error if necessary."
  (when rank-supplied-p
    (assert (= rank (length permutation)) ()
            'permutation-incompatible-rank ))
  (assert (every #'plusp (permutation-flags permutation)) ()
          'permutation-incompatible-rank))

(defun complement-permutation (permutation rank)
  "Return a list of increasing indices that complement PERMUTATION, ie form a
permutation when appended.  Atoms are accepted and treated as lists of a
single element."
  (loop for f across (permutation-flags (ensure-list permutation) rank)
        for index from 0
        when (zerop f)
        collect index))

(defun complete-permutation (permutation rank)
  "Return a completed version of permutation, appending it to its complement."
  (let ((permutation (ensure-list permutation)))
    (append permutation (complement-permutation permutation rank))))

(defun invert-permutation (permutation)
  "Invert a permutation."
  (check-permutation permutation)
  (coerce (let ((result (make-array (length permutation) :element-type 'fixnum)))
            (map nil (let ((index 0))
                       (lambda (p)
                         (setf (aref result p) index)
                  (incf index)))
                 permutation)
            result)
          'list))

(defun identity-permutation-p (permutation
                               &optional (rank (length permutation)))
  "Test if PERMUTATION is the identity permutation, ie a sequence of
consecutive integers starting at 0.  Note that permutation is otherwise not
checked, ie it may not be a permutation."
  (let ((index 0))
    (and
     (every
      (lambda (p)
        (prog1 (= index p)
          (incf index)))
      permutation)
     (= index rank))))

;; Alias to deprecated name for identity-permutation-p
(setf (fdefinition 'identity-permutation?) #'identity-permutation-p)

(defun permute (permutation array)
  "Return ARRAY with the axes permuted by PERMUTATION, which is a sequence of
indexes.  Specifically, an array A is transformed to B, where

  B[b_1,...,b_n] = A[a_1,...,a_n] with b_i=a_{P[i]}

P is the permutation.

Array element type is preserved."
  (let ((rank (array-rank array)))
    (if (identity-permutation-p permutation rank)
        array
        (let ((dimensions (array-dimensions array)))
          (flet ((map-subscripts (subscripts-vector)
                   (map 'list (curry #'aref subscripts-vector) permutation)))
            (check-permutation permutation rank)
            (let ((result (make-array (map-subscripts (coerce dimensions 'vector))
                                      :element-type (array-element-type array))))
              (walk-subscripts (dimensions subscripts position)
                (setf (apply #'aref result (map-subscripts subscripts))
                      (row-major-aref array position)))
              result))))))


;;; each

(defun each* (element-type function array &rest other-arrays)
  "Apply function to the array arguments elementwise, and return the result as
an array with the given ELEMENT-TYPE.  Arguments are checked for dimension
compatibility."
  (assert (apply #'same-dimensions-p array other-arrays))
  (let ((result (make-array (array-dimensions array)
                            :element-type element-type)))
    (apply #'map-into (flatten result) function
           (flatten array) (mapcar #'flatten other-arrays))
    result))

(defun each (function array &rest other-arrays)
  "Like EACH*, with ELEMENT-TYPE T."
  (apply #'each* t function array other-arrays))

;;; margin

(defun margin* (element-type function array inner
                &optional (outer (complement-permutation inner
                                                         (array-rank array))))
  "PERMUTE ARRAY with `(,@OUTER ,@INNER), split the inner subarrays, apply
FUNCTION to each, return the results in an array of dimensions OUTER, with the
given ELEMENT-TYPE."
  (let ((outer (ensure-list outer)))
    (each* element-type function
           (split (permute (append outer (ensure-list inner)) array)
                  (length outer)))))

(defun margin (function array inner
               &optional (outer (complement-permutation inner
                                                        (array-rank array))))
  "Like MARGIN*, with ELEMENT-TYPE T."
  (margin* t function array inner outer))

;;; recycle

(defun recycle (object &key inner outer
                            (element-type (if (arrayp object)
                                              (array-element-type object)
                                              t)))
  "Recycle elements of OBJECT, extending the dimensions by outer (repeating
OBJECT) and inner (repeating each element of OBJECT).  When both INNER and
OUTER are nil, the OBJECT is returned as is.  Non-array OBJECTs are intepreted
as rank 0 arrays, following the usual semantics."
  (if (or inner outer)
      (let ((inner (ensure-dimensions inner))
            (outer (ensure-dimensions outer)))
        (if (arrayp object)
            (let* ((dimensions (array-dimensions object))
                   (result (make-array (append outer dimensions inner)
                                       :element-type element-type))
                   (outer-size (product outer))
                   (size (product dimensions))
                   (inner-size (product inner))
                   (reshaped (reshape result (list outer-size size inner-size))))
              (loop for outer-index below outer-size
                    do (loop for index below size
                             do (fill (sub reshaped outer-index index)
                                      (row-major-aref object index))))
              result)
            (make-array (append outer inner) :initial-element object
                                             :element-type element-type)))
      object))

;;; outer product

(defun outer* (element-type function &rest arrays)
  "Generalized outer product of ARRAYS with FUNCTION.  The resulting array has the concatenated dimensions of ARRAYS, and the given ELEMENT-TYPE."
  (assert arrays)
  (let* ((result (make-array (mapcan #'dims arrays) :element-type element-type))
         (vectors (mapcar #'flatten arrays))
         (flat-dimensions (mapcar #'length vectors))
         (flat-result (reshape result flat-dimensions)))
    (walk-subscripts (flat-dimensions subscripts position)
      (setf (row-major-aref flat-result position)
            (apply function (map 'list (lambda (v s) (aref v s)) vectors subscripts))))
    ;; Note: Using #'aref rather than (lambda (v s) (aref v s)) leads to error
    result))


(defun outer (function &rest arrays)
  "Like OUTER, with ELEMENT-TYPE t."
  (apply #'outer* t function arrays))

;;; vectorize
;;;
;;; vectorize! contains most of the code, which fills a given array.
;;; vectorize* creates an array with given element type, then calls vectorize!
;;; vectorize calls vectorize*, specifying element type T

(defmacro vectorize! (result variables &body body)
  "Fills an array RESULT with the result of
   an array expression. All input and outputs have the same
   shape, and BODY is evaluated for each index

   VARIABLES must be a list of symbols bound to arrays.
   Each array must have the same dimensions. These are
   checked at compile and run-time respectively.

       (let ((a #2A((1 2) (3 4)))
             (b (make-array '(2 2))))
           (vectorize! b (a) (+ a 1)))
       -> #2A((2 3) (4 5))

       (let ((a #(1 2 3))
             (b #(4 5 6)))
           (vectorize! b (a b) (+ a (* b 2))))
       -> #(9 12 15)
   "
  ;; Check that variables is a list of only symbols
  (dolist (var variables)
    (if (not (symbolp var))
        (error "~S is not a symbol" var)))

  (let ((size (gensym))   ; Total array size (same for all variables)
        (result-tmp (gensym)) ; New symbol needed for result, in case the input RESULT is in VARIABLES
        (type (gensym)) ; Element type
        (indx (gensym)))  ; Index inside loop from 0 to size

    ;; Create a new array
    `(let* ((,size (array-total-size ,(first variables)))
            (,result-tmp ,result)  ; result may be an expression. once-only
            (,type (array-element-type ,result-tmp)))
       ;; Check that all variables have the same size
       ,@(mapcar (lambda (var) `(if (not (equal (array-dimensions ,(first variables))
                                                (array-dimensions ,var)))
                                    (error "~S and ~S have different dimensions" ',(first variables) ',var)))
              (rest variables))
       ;; Check dimensions of result done last, to avoid confusing errors
       ;; from incompatible arrays passed to vectorize or vectorize*
       ;; in which cases result is a gensym.
       (if (not (equal (array-dimensions ,(first variables))
                       (array-dimensions ,result-tmp)))
           (error "~S and ~S have different dimensions" ',(first variables) ',result))

       (dotimes (,indx ,size)
         ;; Locally redefine variables to be scalars at a given index
         (let ,(mapcar (lambda (var) (list var `(row-major-aref ,var ,indx))) variables)
           ;; User-supplied function body now evaluated for each index in turn
           (setf (row-major-aref ,result-tmp ,indx) (coerce (progn ,@body) ,type))))
       ,result-tmp)))

(defmacro vectorize* (element-type variables &body body)
  "Makes a new array of type ELEMENT-TYPE, containing the result of
   an array expression. All input and outputs have the same
   shape, and BODY is evaluated for each index

   VARIABLES must be a list of symbols bound to arrays.
   Each array must have the same dimensions. These are
   checked at compile and run-time respectively.

       (let ((a #2A((1 2) (3 4))))
           (vectorize* t (a) (+ a 1)))
       -> #2A((2 3) (4 5))

       (let ((a #(1 2 3))
             (b #(4 5 6)))
           (vectorize* t (a b) (+ a (* b 2))))
       -> #(9 12 15)
   "
  (let ((result (gensym))) ; Returned array
    ;; Create a new array
    `(let ((,result (make-array (array-dimensions ,(first variables))
                                :element-type ,element-type)))
       (vectorize! ,result ,variables ,@body))))

(defmacro vectorize (variables &body body)
    "Makes a new array of type ELEMENT-TYPE, containing the result of
   an array expression. All input and outputs have the same
   shape, and BODY is evaluated for each index

   VARIABLES must be a list of symbols bound to arrays.
   Each array must have the same dimensions. These are
   checked at compile and run-time respectively.

       (let ((a #2A((1 2) (3 4))))
           (vectorize (a) (+ a 1)))
       -> #2A((2 3) (4 5))

       (let ((a #(1 2 3))
             (b #(4 5 6)))
           (vectorize (a b) (+ a (* b 2))))
       -> #(9 12 15)
   "
    `(vectorize* t ,variables ,@body))

(defun across (input function &key (elements nil) (indices nil) (dimensions (dims input)))
  "Iterate across a range of elements in an array INPUT, with the option of specifying which elements within each dimension to process.

The given FUNCTION should take two arguments: A value and the coordinate (index):

  (let ((array (make-array '(2 2))))
    (across array (lambda (val coord) (format t \"~a at ~a~%\" val coord))))
"
  (let* ((proceeding t)
	 (first-of-elements (first elements))
	 (this-range (if (listp first-of-elements)
			 first-of-elements (list first-of-elements))))
    (loop :for elix :in (if this-range this-range (iota (nth (length indices) dimensions))) :while proceeding
       :do (let ((coords (append indices (list elix))))
	     ;; if the halt-if-true value is output by the function, traversal across the array will end
	     ;; by means of nullifying the proceeding variable; this will result in a nil return value
	     ;; from the across function which will stop its recursive parents
	     (if (< (length indices) (1- (length dimensions)))
		 (if (not (across input function :elements (rest elements) :dimensions dimensions :indices coords))
		     (setq proceeding nil))
		 (multiple-value-bind (output halt-if-true)
		     (funcall function (apply #'aref input coords)
			      coords)
		   (declare (ignore output))
		   (if halt-if-true (setq proceeding nil))))))
    proceeding))
