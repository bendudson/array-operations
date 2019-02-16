;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; utilities used internally, not exported

(defun product (dimensions)
  "Product of elements in the argument.  NOT EXPORTED."
  (reduce #'* dimensions))

(define-modify-macro multf (&rest values) * "Multiply by the arguments")

(defun same-dimensions? (array &rest arrays)
  "Test if arguments have the same dimensions.  NOT EXPORTED."
  (let ((dimensions (array-dimensions array)))
    (every (lambda (array)
             (equal dimensions (array-dimensions array)))
           arrays)))

(defun ensure-dimensions (object)
  "Return a list of dimensions corresponding to OBJECT.  Positive integers are
treated as dimensions of rank 1, lists are returned as they are, and arrays
are queried for their dimensions.

OBJECTS accepted by this function as valid dimensions are called `dimension
specifications' in this library."
  (anaphora:aetypecase object
    ((integer 0) (list anaphora:it))
    (list anaphora:it)
    (array (array-dimensions anaphora:it))))

(defmacro walk-subscripts ((dimensions subscripts
                            &optional (position (gensym "POSITION")))
                           &body body)
  "Iterate over the subscripts of an array with given DIMENSIONS.  SUBSCRIPTS
contains the current subscripts as a vector of fixnums, POSITION has the
row-major index.  Consequences are undefined if either POSITION or SUBSCRIPTS
is modified."
  (check-type position symbol)
  (check-type subscripts symbol)
  (with-unique-names (rank last increment dimensions-var)
    `(let* ((,dimensions-var (ensure-dimensions ,dimensions))
            (,rank (length ,dimensions-var))
            (,dimensions-var (make-array ,rank
                                         :element-type 'fixnum
                                         :initial-contents ,dimensions-var))
            (,last (1- ,rank))
            (,subscripts (make-array ,rank
                                     :element-type 'fixnum
                                     :initial-element 0)))
       (labels ((,increment (index)
         (unless (minusp index)
           (when (= (incf (aref ,subscripts index))
                    (aref ,dimensions-var index))
             (setf (aref ,subscripts index) 0)
             (,increment (1- index))))))
         (dotimes (,position (product ,dimensions-var))
           ,@body
           (,increment ,last))))))

(defmacro walk-subscripts-list ((dimensions subscripts
                                 &optional (position (gensym "POSITION")))
                                &body body)
  "Like WALK-SUBSCRIPTS, but SUBSCRIPTS is a newly created list for each
position that does not share structure and can be freely used/modified/kept
etc."
  (with-unique-names (subscripts-vector)
    `(walk-subscripts (,dimensions ,subscripts-vector ,position)
       (let ((,subscripts (coerce ,subscripts-vector 'list)))
         ,@body))))


(defmacro nested-loop (syms dimensions &body body)
  "Iterates over a multidimensional range of indices.

   SYMS must be a list of symbols, with the first symbol
   corresponding to the outermost loop.

   DIMENSIONS will be evaluated, and must be a list of
   dimension sizes, of the same length as SYMS.

   Example:
    (nested-loop (i j) '(10 20) (format t '~a ~a~%' i j))

   expands to:

    ; Check dimensions
    (destructuring-bind (g1 g2) '(10 20)
        (loop for i from 0 below g1 do
            (loop for j from 0 below g2 do
                (format t '~a ~a~%' i j))))

  with some additional type and dimension checks.
  "
  (unless syms (return-from nested-loop `(progn ,@body))) ; No symbols

  ;; Generate gensyms for dimension sizes
  (let* ((rank (length syms))
         (syms-rev (reverse syms)) ; Reverse, since starting with innermost
         (dims-rev (loop for i from 0 below rank collecting (gensym))) ; innermost dimension first
         (result `(progn ,@body))) ; Start with innermost expression
    ;; Wrap previous result inside a loop for each dimension
    (loop for sym in syms-rev for dim in dims-rev do
         (unless (symbolp sym) (error "~S is not a symbol. First argument to nested-loop must be a list of symbols" sym))
         (setf result
               `(loop for ,sym from 0 below ,dim do
                     ,result)))
    ;; Add checking of rank and dimension types, and get dimensions into gensym list
    (let ((dims (gensym)))
      `(let ((,dims ,dimensions))
         (unless (= (length ,dims) ,rank) (error "Incorrect number of dimensions: Expected ~a but got ~a" ,rank (length ,dims)))
         (dolist (dim ,dims)
           (unless (integerp dim) (error "Dimensions must be integers: ~S" dim)))
         (destructuring-bind ,(reverse dims-rev) ,dims ; Dimensions reversed so that innermost is last
           ,result)))))

