;;;; Generic functions for elementary array operations, with methods on 'array.
;;;;
;;;; Used abundantly in this library.
;;;; Invites new methods to enable treating other data structures as arrays.

(defpackage :array-operations/generic
  (:use :cl :let-plus)
  (:export :as-array
           :element-type
           :dims
           :size
           :rank
           :dim
           :&dims  ;; For let+
           :nrow
           :ncol))

(in-package :array-operations/generic)

;;; shorthand functions
;;;
;;; In order to have the library functions work on objects other than arrays,
;;;
;;; 1. AS-ARRAY _must_ be defined,
;;; 2. ELEMENT-TYPE and DIMS _should_ be defined when AS-ARRAY is costly (eg conses),
;;; 3. all the other methods are optional as they fall back to DIMS.

(defgeneric as-array (object)
  (:documentation "Return the contents of OBJECT as an array.  Exact semantics depends on OBJECT, but generally objects which contain elements in a rectilinear coordinate system should have a natural mapping to arrays.

When the second value is T, the array itself does not share structure with OBJECT, but its elements may.  Otherwise, it is indeterminate whether the two objects share structure, and consequences of modifying the result are not defined.  Methods are encouraged but not required to return a second value.")
  (:method ((array array))
    array)
  (:method (object)
    (make-array nil :initial-element object)))

(defgeneric element-type (array)
  (:documentation "Return TYPE such that

1. all elements of ARRAY are guaranteed to be a subtype of TYPE,

2. if applicable, elements of ARRAY can be set to values which are of a type that is a subtype of TYPE.")
  (:method ((array array))
    (array-element-type array))
  (:method (array)
    (array-element-type (as-array array))))

(defgeneric dims (array)
  (:documentation "Return a list of dimensions.

For non-array objects, SIZE, DIM, NROW and NCOL use this method by default, so it is enough to define it (unless efficiency is a concern).

When DIMS is not defined for an object, it falls back to as-array, which may be very inefficient for objects which need to be consed.  It is always advisable to define DIMS.")
  (:method ((array array))
    (array-dimensions array))
  (:method (array)
    (array-dimensions (as-array array))))

;; "let-plus" has been removed as a dependency of the code in this library,
;; but is needed by some other libraries which depend on ARRAY-OPERATIONS.
(define-let+-expansion (&dims dimensions :value-var value-var
                                        :body-var body-var)
 "Dimensions of array-like object."
 `(let+ ((,dimensions (dims ,value-var)))
    ,@body-var))

(defgeneric size (array)
  (:documentation "Return the total number of elements in array.")
  (:method ((array array))
    (array-total-size array))
  (:method (array)
    (reduce #'* (dims array))))

(defgeneric rank (array)
  (:documentation "Return the rank of ARRAY.")
  (:method ((array array))
    (array-rank array))
  (:method (array)
    (length (dims array))))

(defgeneric dim (array axis)
  (:documentation "Return specificed dimension of ARRAY.")
  (:method ((array array) axis)
    (array-dimension array axis))
  (:method (array axis)
    ;; NOTE: ELT is preferred to NTH here because it signals an error for invalid axes
    (elt (dims array) axis)))

(defgeneric nrow (array)
  (:documentation "Number of rows.  Will signal an error if ARRAY is not a matrix.")
  (:method ((array array))
    (assert (= (rank array) 2))
    (array-dimension array 0))
  (:method (array)
    (dim array 0)))

(defgeneric ncol (array)
  (:documentation "Number of columns.  Will signal an error if ARRAY is not a matrix.")
  (:method ((array array))
    (assert (= (rank array) 2))
    (array-dimension array 1))
  (:method (array)
    (dim array 1)))
