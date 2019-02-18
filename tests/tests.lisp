;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:array-operations-tests
  (:use #:cl #:alexandria #:clunit)
  (:export #:run))

(cl:in-package #:array-operations-tests)

(defsuite tests ())
(defsuite creation (tests))
(defsuite utilities (tests))
(defsuite displacement (tests))
(defsuite transformations (tests))
(defsuite indexing (tests))
(defsuite stack (tests))

(defun run (&optional interactive?)
  "Run all the tests for array-operations."
  (run-suite 'tests :use-debugger interactive?))

;;; creation

(deftest zeros (creation)
  (let ((result (aops:zeros 4)))
    (assert-equalp '(simple-vector 4) (type-of result))
    (assert-equalp 0 (aref result 1)))
  (let ((result (aops:zeros '(2 2))))
    (assert-equalp '(2 2) (array-dimensions result))
    (assert-equalp 0 (aref result 1 1))))

(deftest zeros* (creation)
  (let ((result (aops:zeros* 'integer 4)))
    (assert-equalp '(simple-vector 4) (type-of result))
    (assert-equalp 0 (aref result 1)))
  (let ((result (aops:zeros* 'single-float 4)))
    (assert-equalp '(simple-array single-float (4)) (type-of result))
    (assert-equalp 0.0 (aref result 1)))
  (let ((result (aops:zeros* 'double-float '(2 2))))
    (assert-equalp '(2 2) (array-dimensions result))
    (assert-equalp '(simple-array double-float (2 2)) (type-of result))
    (assert-equalp 0d0 (aref result 1 1))))

(deftest zeros! (creation)
  (let ((array (make-array '(2 3)
                           :element-type 'single-float
                           :initial-element 1.2)))
    (aops:zeros! array)
    (dotimes (i (array-total-size array))
      (assert-equalp 0s0 (row-major-aref array i)))))

(deftest ones (creation)
  (let ((result (aops:ones 4)))
    (assert-equalp '(simple-vector 4) (type-of result))
    (assert-equalp (aref result 1) 1))
  (let ((result (aops:ones '(2 3 2))))
    (assert-equalp '(2 3 2) (array-dimensions result))
    (assert-equalp 1 (aref result 1 1 0))))

(deftest ones* (creation)
  (let ((result (aops:ones* 'integer 4)))
    (assert-equalp '(simple-vector 4) (type-of result))
    (assert-equalp (aref result 1) 1))
  (let ((result (aops:ones* 'single-float 4)))
    (assert-equalp '(simple-array single-float (4)) (type-of result))
    (assert-equalp (aref result 1) 1.0))
  (let ((result (aops:ones* 'double-float '(2 2))))
    (assert-equalp '(2 2) (array-dimensions result))
    (assert-equalp '(simple-array double-float (2 2)) (type-of result))
    (assert-equalp 1d0 (aref result 1 1))))

(deftest ones! (creation)
  (let ((array (make-array 3
                           :element-type 'integer
                           :initial-element 3)))
    (aops:ones! array)
    (dotimes (i (array-total-size array))
      (assert-equalp 1 (row-major-aref array i)))))

(deftest rand (creation)
  (let ((a (aops:rand 3)))
    (assert-equalp '(3) (array-dimensions a))
    (assert-true (not (equal (aref a 1) (aref a 2))) a)
    (let ((b (aops:rand 3)))
      (assert-false (equal (aref a 2) (aref b 2)) a b)))
  (let* ((c (aops:rand 1000))
         (c-max (reduce #'max c))
         (c-min (reduce #'min c)))
    (assert-true (>= c-min 0.0))
    (assert-true (<= c-max 1.0))
    (assert-true (> c-max c-min))))

(deftest rand! (creation)
  (let ((array (make-array 3
                           :element-type 'double-float
                           :initial-element 3d0)))
    (aops:rand! array)
    (assert-false (= (aref array 1) (aref array 2)))
    (assert-true (>= (reduce #'min array) 0.0))
    (assert-true (<= (reduce #'max array) 1.0))))

(deftest rand* (creation)
  (let ((a (aops:rand* 'double-float 3)))
    (assert-equalp '(3) (array-dimensions a))
    (assert-true (not (equal (aref a 1) (aref a 2))) a)
    (let ((b (aops:rand* 'double-float 3)))
      (assert-false (equal (aref a 2) (aref b 2)) a b)))
  (let* ((c (aops:rand* 'double-float  1000))
         (c-max (reduce #'max c))
         (c-min (reduce #'min c)))
    (assert-true (>= c-min 0.0))
    (assert-true (<= c-max 1.0))
    (assert-true (> c-max c-min))))

(deftest randn (creation)
  (let ((a (aops:randn 3)))
    (assert-equalp '(3) (array-dimensions a))
    (assert-true (not (equal (aref a 1) (aref a 2))) a)
    (let ((b (aops:randn 3)))
      (assert-false (equal (aref a 2) (aref b 2)) a b)))
  (let* ((c (aops:randn 1000))
         (c-max (reduce #'max c))
         (c-min (reduce #'min c)))
    (assert-true (< c-min 0.0))
    (assert-true (> c-max 0.0))))

(deftest randn* (creation)
  (let ((a (aops:randn* 'double-float 3)))
    (assert-equalp '(3) (array-dimensions a))
    (assert-true (not (equal (aref a 1) (aref a 2))) a)
    (let ((b (aops:randn* 'double-float 3)))
      (assert-false (equal (aref a 2) (aref b 2)) a b)))
  (let* ((c (aops:randn* 'double-float 1000))
         (c-max (reduce #'max c))
         (c-min (reduce #'min c)))
    (assert-true (< c-min 0.0))
    (assert-true (> c-max 0.0))))

(deftest randn! (creation)
  (let ((a (make-array '(2 2) :element-type 'single-float)))
    (let ((b (aops:randn! a)))
      (assert-equalp a b)
      (assert-false (= (aref a 0 1) (aref a 1 1))))))

(deftest linspace (creation)
  (assert-equalp #(0 1 2 3) (aops:linspace 0 3 4))
  (assert-equalp #(1 3/2 2 5/2 3) (aops:linspace 1 3 5))
  (assert-equalp #(0.0d0 2.0d0 4.0d0) (aops:linspace 0 4d0 3)))

(deftest linspace! (creation)
  (let ((a (make-array 5 :element-type 'single-float)))
    (aops:linspace! a 1 5)
    (assert-equalp #(1 2 3 4 5) a)
    (aops:linspace! a 0 1)
    (assert-equalp #(0.0 0.25 0.5 0.75 1.0) a)))

(deftest similar-array (creation)
  (let* ((a #(1 2 3 4))
         (a* (aops:similar-array a)))
    (assert-equalp '(4) (array-dimensions a*))
    (assert-true (typep (array-element-type a*) 'T)))

  (let* ((b (make-array '(2 2) :element-type 'fixnum))
         (b* (aops:similar-array b))
         (c (aops:similar-array b :element-type 'double-float))
         (d (aops:similar-array b :adjustable t)))
    (assert-equalp '(2 2) (array-dimensions b*))
    (assert-equalp (array-element-type b*) 'FIXNUM)
    (assert-equalp (array-element-type c) 'DOUBLE-FLOAT)
    (assert-true (adjustable-array-p d))))

;;; utilities

(deftest walk-subscripts (utilities)
  (let (result)
    (aops:walk-subscripts ('(2 3) subscripts position)
      (push (cons position (copy-seq subscripts)) result))
    (assert-equalp '((0 . #(0 0))
                     (1 . #(0 1))
                     (2 . #(0 2))
                     (3 . #(1 0))
                     (4 . #(1 1))
                     (5 . #(1 2)))
        (reverse result))))

(deftest nested-loop (utilities)
  (let (result)
    (aops:nested-loop (i j) '(2 3)
      (push (list i j) result))
    (assert-equalp '((0 0)
                     (0 1)
                     (0 2)
                     (1 0)
                     (1 1)
                     (1 2))
        (reverse result)))

  ; Test dimensions of local scope variable (no eval)
  (let ((a #(1 2 3 4))
        result)
    (aops:nested-loop (i) (array-dimensions a)
      (push i result))
    (assert-equalp '(0 1 2 3) (reverse result))))



;;; displacement

(deftest displacement (displacement)
  (let ((a #2A((0 1) (2 3) (4 5))))
    ;; displace
    (assert-equalp #(0 1) (aops:displace a 2))
    (assert-equalp #2A((2 3)) (aops:displace a '(1 2) 2))
    ;; flatten
    (assert-equalp #(0 1 2 3 4 5) (aops:flatten a))
    ;; split
    (assert-equalp a (aops:split a 0))
    (assert-equalp #(#(0 1) #(2 3) #(4 5)) (aops:split a 1))
    (assert-equalp a (aops:split a 2))
    ;; sub
    (assert-equalp #(4 5) (aops:sub a 2))
    (assert-equalp 4 (aops:sub a 2 0))
    (let ((b (copy-array a)))
      (assert-equalp #(7 9) (setf (aops:sub b 1) #(7 9)))
      (assert-equalp #2A((0 1) (7 9) (4 5)) b)
      (assert-condition error (setf (aops:sub 0 2) #(1))))
    ;; partition
    (assert-equalp #2A((2 3) (4 5)) (aops:partition a 1))
    (assert-equalp #2A((2 3)) (aops:partition a 1 2))
    (assert-condition error (aops:partition a 0 9))
    (let ((b (copy-array a)))
      (setf (aops:partition b 1) #2A((11 13) (17 19)))
      (assert-equalp #2A((0 1) (11 13) (17 19)) b))
    ;; combine
    (assert-equalp a (aops:combine (aops:split a 0)))
    (assert-equalp a (aops:combine (aops:split a 1)))
    (assert-equalp a (aops:combine (aops:split a 2)))
    (let ((b #(1 #(2 3) 4))
          (c 9))
      (assert-equalp b (aops:combine b))
      (assert-equalp c (aops:combine c)))
    ;; subvec
    (let ((b (copy-array (aops:flatten a))))
      (assert-equalp #(2 3 4 5) (aops:subvec b 2))
      (assert-equalp #(3 4) (aops:subvec b 3 5))
      (assert-condition error (aops:subvec b 0 9))
      (assert-equalp #(7 9) (setf (aops:subvec b 3 5) #(7 9)))
      (assert-equalp #(0 1 2 7 9 5) b)
      (assert-condition error (setf (aops:subvec b 3 5) #(7))))
    ;; fill-in-dimensions, a helper function to reshape
    (assert-equalp '(2) (aops::fill-in-dimensions 2 2))
    (assert-equalp '(2) (aops::fill-in-dimensions #(1 2) 2))
    (assert-equalp '(5 4) (aops::fill-in-dimensions '(5 4) 20))
    (assert-equalp '(5 4) (aops::fill-in-dimensions '(5 t) 20))
    (assert-condition error (aops::fill-in-dimensions 5 7))
    (assert-condition error (aops::fill-in-dimensions #(2 3) 7))
    (assert-condition error (aops::fill-in-dimensions '(2 3) 7))
    (assert-condition error (aops::fill-in-dimensions '(t t) 7))
    ;; reshape & variances
    (assert-equalp #2A((0 1 2) (3 4 5)) (aops:reshape a '(2 3)))
    (assert-equalp #2A((0 1 2 3 4 5)) (aops:reshape-row a))
    (assert-equalp #2A((0) (1) (2) (3) (4) (5)) (aops:reshape-col a))))

;;; transformations

(deftest fill! (transformations)
  (let ((a (make-array '(3 2) :initial-element 1.0)))
    (aops:fill! a 0)
    (assert-equalp a #2A((0.0 0.0)
                         (0.0 0.0)
                         (0.0 0.0)))))

(deftest coercing (transformations)
  (assert-equality (curry #'every #'eql)
      #(1d0 2d0 3d0)
      (map 'vector (aops:coercing 'double-float) #(1 2 3)))
  (assert-equality (curry #'every #'eql)
      #(1d0 4d0 9d0)
      (map 'vector (aops:coercing 'double-float (lambda (x) (* x x))) #(1 2 3))))

(deftest generate (transformations)
  (let ((a (aops:generate #'identity '(3 2) :position))
        (b (aops:generate #'identity '(2 3) :subscripts)))
    (assert-equalp #2A((0 1)
                       (2 3)
                       (4 5))
      a)
    (assert-equalp #2A(((0 0) (0 1) (0 2))
                       ((1 0) (1 1) (1 2)))
      b)
    (assert-equalp #2A(((0 0 0) (1 0 1)))
      (aops:generate #'cons '(1 2) :position-and-subscripts))))

(defun permute% (subscripts-mapping array)
  "Helper function for testing permutation.  Permutes ARRAY using SUBSCRIPTS-MAPPING, should return the permuted arguments as a list."
  (let ((dimensions (array-dimensions array)))
    (flet ((map% (subscripts)
             (apply subscripts-mapping subscripts)))
      (let ((result (make-array (map% dimensions)
                                :element-type (array-element-type array))))
        (aops:walk-subscripts-list (dimensions subscripts)
          (setf (apply #'aref result (map% subscripts))
                (apply #'aref array subscripts)))
        result))))

(deftest permutations (transformations)
  (assert-equalp #*10110 (aops::permutation-flags '(0 3 2) 5))
  (assert-condition error (aops::check-permutation '(0 1 1)))
  (assert-equalp '(0 1 4) (aops:complement-permutation '(3 2) 5))
  (assert-equalp '(3 2 0 1 4) (aops:complete-permutation '(3 2) 5))
  (assert-equalp '(0 1 2 3) (aops:invert-permutation '(0 1 2 3)))
  (assert-equalp '(1 3 2 0) (aops:invert-permutation '(3 0 2 1)))
  (flet ((assert-equalp-i2 (permutation)
           (assert-equalp permutation
               (aops:invert-permutation (aops:invert-permutation permutation)))))
    (assert-equalp-i2 '(0 1 2 3))
    (assert-equalp-i2 '(3 0 2 1))))

(deftest permute (transformations)
  (let ((a (aops:generate #'identity '(3 2) :position)))
    (assert-equalp a (aops:permute '(0 1) a))
    (assert-equalp  #2A((0 2 4)
                        (1 3 5))
      (aops:permute '(1 0) a))
    (assert-condition aops:permutation-repeated-index (aops:permute '(0 0) a))
    (assert-condition aops:permutation-invalid-index (aops:permute '(2 0) a))
    (assert-condition aops:permutation-incompatible-rank (aops:permute '(0) a)))
  (let ((p (alexandria:shuffle (list 0 1 2 3 4)))
        (a (aops:generate (lambda () (random 100)) '(2 3 4 5 6)))
        (*lift-equality-test* #'equalp))
    (assert-equalp p (aops:invert-permutation (aops:invert-permutation p)))
    (assert-equalp a (aops:permute (aops:invert-permutation p) (aops:permute p a))))
  (let ((a (aops:generate #'identity '(2 2 2) :position)))
    (assert-equalp (aops:permute '(2 0 1) a)
        (permute% (lambda (a b c) (list c a b)) a))))

(deftest each (transformations)
  (let ((a (aops:generate #'identity '(2 5) :position)))
    (assert-equalp (aops:generate #'1+ '(2 5) :position) (aops:each #'1+ a)))
  (assert-equalp #(1 1 2 3) (aops:each #'- #(2 3 5 7) #(1 2 3 4))))

(deftest margin (transformations)
  (let ((a (aops:generate #'identity '(3 5) :position)))
    (assert-equalp #(10 35 60) (aops:margin (curry #'reduce #'+) a 1))
    (assert-equalp #(0 66 168 312 504) (aops:margin (curry #'reduce #'*) a 0))))

(deftest recycle (transformations)
  (assert-equalp (make-array '(3 4 2 1) :initial-element 1)
      (aops:recycle 1 :inner '(2 1) :outer '(3 4)))
  (let ((a (aops:generate #'identity '(2 3) :position)))
    (assert-equalp a (aops:recycle a))
    (assert-equalp (aops:generate (lambda (p) (floor p 2)) '(2 3 2) :position)
        (aops:recycle a :inner 2))
    (assert-equalp (aops:generate (lambda (p) (rem p 6)) '(2 2 3 1) :position)
        (aops:recycle a :inner 1 :outer 2))))

(deftest outer (transformations)
  (let ((a #(2 3 5))
        (b #(7 11))
        (c #2A((7 11)
               (13 17))))
    (assert-equalp #2A((14 22)
                       (21 33)
                       (35 55))
      (aops:outer #'* a b))
    (assert-equalp #3A(((14 21 35) (22 33 55))
                       ((26 39 65) (34 51 85)))
      (aops:outer #'* c a))
    (assert-equalp (aops:combine (aops:each (lambda (v)
                                          (aops:each (curry #'* v) c))
                                        a))
      (aops:outer #'* a c))))


(deftest vectorize! (transformations)
  (let ((a #2A((1 2) (3 4)))
        (b (make-array '(2 2))))
    (assert-equalp #2A((2 3) (4 5))
      (aops:vectorize! b (a) (+ a 1))
      "vectorize! return value")
    (assert-equalp #2A((2 3) (4 5)) b
                   "vectorize! modified first arg")
    (assert-equalp #2A((1 2) (3 4)) a
                   "vectorize! didn't modify operand"))

  (let ((a #2A((1 2) (3 4)))
        (b (make-array 4)))
    (assert-condition error
        (aops:vectorize! b (a) (+ a 1))
      "Wrong result array shape"))

  (let ((a #(1 2 3))
        (b #(4 5 6)))
    (let ((c (make-array 3 :element-type 'integer)))
      (assert-equalp #(9 12 15)
                     (aops:vectorize! c (a b) (+ a (* b 2)))
                     "vectorize! return value")
      (assert-equalp #(9 12 15) c
                     "vectorize! modified first arg"))
    (let ((c (make-array 4 :element-type 'integer)))
      (assert-condition error
          (aops:vectorize! c (a b) (+ a (* b 2)))
          "Wrong result array shape")))

  ;; Check that an expression can be passed as first argument
  ;; without being evaluated multiple times
  (let ((a #(1 2 3))
        (b #(4 5 6)))

    (let ((count 0)) ; Count how many times make-array is called
      (assert-equalp #(9 12 15)
          (aops:vectorize! (progn
                             (incf count)
                             (make-array 3 :element-type 'integer))
              (a b) (+ a (* b 2))))

      (assert-equalp 1 count "Expression evaluated multiple times"))))


(deftest vectorize* (transformations)
  (let ((a #2A((1 2) (3 4)))
        (b (make-array '(2 2))))
    (assert-equalp #2A((2 3) (4 5))
                   (aops:vectorize* 'integer (a) (+ a 1))))
  (let ((a #(1 2 3))
        (b #(4 5 6)))
    (assert-equalp #(9.0 12.0 15.0)
                   (aops:vectorize* 'single-float (a b) (+ a (* b 2))))))

(deftest vectorize (transformations)
  (let ((a #2A((1 2) (3 4)))
        (b (make-array '(2 2))))
    (assert-equalp #2A((2 3) (4 5))
                   (aops:vectorize (a) (+ a 1))))
  (let ((a #(1 2 3))
        (b #(4 5 6)))
    (assert-equalp #(9 12 15)
                   (aops:vectorize (a b) (+ a (* b 2))))))

(deftest vectorize-reduce (transformations)
  (let ((a #2A((1 2) (3 4)))
        (b #2A((1 3) (5 4))))
    (assert-equalp 2
                   (aops:vectorize-reduce #'max (a b) (abs (- a b))))))

;;; indexing

(deftest each-index (indexing)
  (let ((a #2A((1 2 3) (4 5 6)))
        (b #2A((1 2) (3 4))))

    ;; Transpose
    (assert-equalp
        #2A((1 4) (2 5) (3 6))
        (aops:each-index (i j) (aref a j i)))

    ;; Diagonal
    (assert-equalp
        #(1 4)
        (aops:each-index i (aref b i i)))

    ;; Checks dimensions
    (assert-condition error
        (aops:each-index i (aref a i i)))

    ;; Arrays of arrays
    (assert-equalp
        #( #(1 2 3) #(4 5 6) )
        (aops:each-index i
          (aops:each-index j
            (aref A i j))))

    ;; Matrix-matrix multiply
    (assert-equalp
        #2A((9 12 15) (19 26 33))
      (aops:each-index (i j)
        (aops:sum-index k
          (* (aref B i k) (aref A k j))))))

  ;; Indexing with ELT and SVREF
  (let ((a #(1 2 3)))
    (assert-equalp
     #(1 2 3)
     (aops:each-index i (elt a i)))

    (assert-equalp
     #(1 2 3)
     (aops:each-index i (svref a i)))))

(deftest each-index* (indexing)
  (let ((a #(1 2 3))
        (b #2A((1 2) (3 4))))

    ;; Test element type (fixnum)
    (assert-equalp
        'fixnum
        (array-element-type (aops:each-index* 'fixnum i (aref a i))))

    ;; Test whole type, 2x2 array
    (assert-true
        (typep
         (aops:each-index* 'single-float (i j) (aref b i j))
         '(SIMPLE-ARRAY SINGLE-FLOAT (2 2))))))

(deftest each-index! (indexing)
  (let ((a (make-array '(2 3))))

    ;; Returns the result
    (assert-equalp
     #2A((0 1 2) (-1 0 1))
     (aops:each-index! a (i j) (- j i)))

    ;; Also modifies A
    (assert-equalp
     #2A((0 1 2) (-1 0 1))
      a))

  ;; First argument is evaluated only once
  (let ((count 0))
    (assert-equalp
        #2A((0 1 2) (-1 0 1))
      (aops:each-index! (progn
                          (incf count)
                          (make-array '(2 3)))
          (i j) (- j i)))
    (assert-equalp 1 count "Expression not evaluated only once")))

(deftest sum-index (indexing)
  (let ((A #2A((1 2) (3 4)))
        (B #2A((1 2 3) (4 5 6))))

    ;; Sum all elements
    (assert-equalp
        10
        (aops:sum-index i (row-major-aref A i)))

    ;; Sum all elements using AREF
    (assert-equalp
        10
        (aops:sum-index (i j) (aref A i j)))

    ;; Trace of array
    (assert-equalp
        5
        (aops:sum-index i (aref A i i)))

    ;; Checks incompatible dimensions
    (assert-condition error
        (aops:sum-index i (aref B i i)))))

(deftest reduce-index (indexing)
  (let ((A #2A((1 2) (3 4)))
        (B #2A((1 2 3) (4 5 6))))

    ;; Sum all elements
    (assert-equalp
        10
        (aops:reduce-index #'+ i (row-major-aref A i)))

    ;; Multiply all elements using AREF
    (assert-equalp
        24
        (aops:reduce-index #'* (i j) (aref A i j)))

    ;; Trace of array
    (assert-equalp
        5
        (aops:reduce-index #'+ i (aref A i i)))

    ;; Maximum in an array
    (assert-equalp
        4
        (aops:reduce-index #'max j (row-major-aref A j)))

    ;; Checks incompatible dimensions
    (assert-condition error
        (aops:reduce-index #'+ i (aref B i i)))))


;;; stack

(deftest stack-rows (stack)
  (let ((a 1)
        (b #(2 3))
        (c #2A((4 5)
               (6 7))))
    (assert-equalp #2A((1 1)
                       (2 3)
                       (4 5)
                       (6 7))
      (aops:stack-rows a b c))
    (assert-equalp #2A((2 3)
                       (1 1)
                       (4 5)
                       (6 7))
      (aops:stack-rows b a c))
    (assert-equalp #2A((4 5)
                       (6 7)
                       (1 1)
                       (2 3))
      (aops:stack-rows c a b))
    (assert-equalp #2A((1) (2) (3)) (aops:stack-rows 1 2 3))
    (assert-condition error (aops:stack-rows #2A((1)) c))))

(deftest stack-cols (stack)
  (let ((a 1)
        (b #(2 3))
        (c #2A((4 5)
               (6 7))))
    (assert-equalp #2A((1 2 4 5)
                       (1 3 6 7))
      (aops:stack-cols a b c))
    (assert-equalp #2A((2 1 4 5)
                       (3 1 6 7))
      (aops:stack-cols b a c))
    (assert-equalp #2A((4 5 1 2)
                       (6 7 1 3))
      (aops:stack-cols c a b))
    (assert-equalp #2A((1 2 3)) (aops:stack-cols 1 2 3))
    (assert-condition error (aops:stack-cols #2A((1)) c))))

(deftest stack0 (stack)
  (assert-equalp #(0 1 2 3 4 5 6) (aops:stack 0 #(0 1 2 3) #(4 5 6)))
  (assert-equalp #2A((0 1)
                     (2 3)
                     (5 7))
    (aops:stack 0
              #2A((0 1)
                  (2 3))
              #2A((5 7))))
  (assert-condition error (aops:stack 0 #(0 1) #2A((0 1 2 3))))
  (assert-condition error (aops:stack 0 #2A((1)) #2A((0 1)))))

(deftest stack (stack)
  (assert-equalp #2A((0 1 5)
                     (2 3 9))
    (aops:stack 1
              #2A((0 1)
                  (2 3))
              #2A((5) (9)))))
