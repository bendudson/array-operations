(in-package #:array-operations)

(defun find-array-dimensions (expr)
  "Walks an expression tree EXPR, finds AREF and ROW-MAJOR-AREF, SVREF or ELT calls.
   Returns a list of (symbol, expr) where EXPR is an expression which
   evaluates to the array dimension size for SYMBOL.

   Example:
     (find-array-dimensions '(+ (aref a i) (* 2 (aref b j k))))

   -> ((I ARRAY-DIMENSION A 0) (K ARRAY-DIMENSION B 1) (J ARRAY-DIMENSION B 0))

  "
  (cond
    ;; If EXPR is not a list, nothing to return
    ((not (listp expr)) nil)

    ;; If EXPR is an AREF
    ((equalp (first expr) 'aref)
     ;; Parse this AREF for indices
     (let ((arr (second expr)))
       (unless (symbolp arr)
         (warn "Array expression ~S will be evaluated multiple times" arr))

       (do ((ind 0 (1+ ind))
            (symlist (cddr expr) (cdr symlist))
            ;; Build list of index constraints (symbol, array, index)
            (result nil (if (symbolp (first symlist))
                            (cons (list (first symlist) 'array-dimension arr ind) result)
                            result)))
           ((not symlist) result))))

    ;; If EXPR is ROW-MAJOR-AREF
    ((equalp (first expr) 'row-major-aref)
     (let ((arr (second expr)))
       (unless (symbolp arr)
         (warn "Array expression ~S will be evaluated multiple times" arr))

       (list (list (third expr) 'array-total-size arr))))

    ;; If EXPR is SVREF or ELT
    ((or (equalp (first expr) 'svref)
         (equalp (first expr) 'elt))
     (let ((arr (second expr)))
       (unless (symbolp arr)
         (warn "Expression ~S will be evaluated multiple times" arr))

       (list (list (third expr) 'length arr))))

    ;; Otherwise, walk elements of the list
    ;; join together the alist
    (t (mapcan #'find-array-dimensions expr))))

(defmacro foreach (&key (index nil) (sum nil) ((:value body) nil))
  "Examples:

   Matrix-matrix multiply

    (foreach :index (i j) :sum k
        :value (* (aref A i k) (aref B k j)))

   Sum over vector

    (foreach :sum i :value (aref A i))
  "
  (let ((dim-exprs (find-array-dimensions body))
        (index (if (listp index) index
                   (list index)))  ; Ensure that INDEX is a list
        (sum (if (listp sum) sum
                 (list sum))))  ; Ensure that SUM is a list
    (flet ((get-dim-expr (sym)
             ;; Get an expression to determine the range of index SYM
             ;; Check that SYM is a symbol
             (unless (symbolp sym) (error "Index must be a symbol ~S" sym))

             ;; Find an expression which sets the range of SYM
             (let ((dim-expr (assoc sym dim-exprs)))
               (unless dim-expr (error "Cannot determine range of index ~S" sym))
               (rest dim-expr))))
      (let ((index-sizes (loop for i from 0 below (length index) collecting (gensym)))
            (sum-sizes (loop for i from 0 below (length sum) collecting (gensym)))
            (let-list nil)   ; let environment
            (result-array (gensym))  ; The array to be returned
            (result body))   ; The result of this macro

        (loop for sym in (reverse sum) for size in sum-sizes do
           ;; Add a dimension to be set in the `let` environment
             (push (list size (get-dim-expr sym)) let-list)
           ;; Wrap a summation loop around the inner expression
             (setf result
                   `(loop for ,sym from 0 below ,size summing
                         ,result)))

        (when index
          ;; Set elements of an array
          (setf result
                `(setf (aref ,result-array ,@index) ,result))

          (loop for sym in (reverse index) for size in index-sizes do
             ;; Add a dimension to be set in the `let` environment
               (push (list size (get-dim-expr sym)) let-list)
             ;; Wrap a loop around result
               (setf result
                     `(loop for ,sym from 0 below ,size do
                           ,result)))
          (setf result
                `(let ((,result-array (make-array (list ,@(reverse index-sizes)))))
                   ,result
                   ,result-array)))

        (list 'let let-list
              result)))))

;;;
;;; More lispy way to iterate over indices
;;;
;;; (each-index (i j) expr)
;;;
;;; and
;;;
;;; (sum-index k expr)
;;;
;;; could be combined e.g.
;;;
;;; (each-index (i j)
;;;   (sum-index k
;;;     (* (aref A i k) (aref B k j))))
;;;

(defmacro each-index* (element-type index &body body)
  "Given one or more symbols INDEX, creates an array
   with ELEMENT-TYPE, then iterates over the index ranges
   with the innermost loop using the last index.
   Each iteration evaluates BODY, and sets the array element.

   To find the range of the indices, walks the BODY expression
   to determine the index ranges by looking for
   AREF and ROW-MAJOR-AREF calls.

  Transpose of 2D array A

    (each-index* t (i j) 
      (aref A j i))

  Diagonal of a square 2D array

    (each-index* t i (aref A i i))

  Turn a 2D array into an array of arrays

    (each-index* t i
      (each-index* t j
        (aref A i j)))

  Outer product of two 1D arrays to create a 2D array

    (each-index* t (i j)
      (* (aref x i) (aref y j)))

  Matrix-vector product:

    (each-index* t i
      (sum-index j
        (* (aref A i j) (aref x j))))

  "
  (let ((dim-exprs (find-array-dimensions body))
        (index (if (listp index) index
                   (list index))))  ; Ensure that INDEX is a list
    ;; Check that all elements of INDEX are symbols
    (dolist (sym index)
      (unless (symbolp sym) (error "Index must be a symbol ~S" sym)))

    (let ((index-sizes (loop for i from 0 below (length index) collecting (gensym)))
          (let-list nil)   ; let environment
          (checks-list nil)  ; Array size checks
          (result-array (gensym))  ; The array to be returned
          (result body))   ; The result of this macro

      ;; Innermost form sets elements in the result array
      (setf result
            `(setf (aref ,result-array ,@index) (coerce (progn ,@result) ,element-type)))

      (loop for sym in (reverse index) for size in index-sizes do
         ;; Add a dimension to be set in the `let` environment
         ;; Find an expression which sets the range of SYM
           (let ((dim-expr (assoc sym dim-exprs)))
             (unless dim-expr (error "Cannot determine range of index ~S" sym))
             (push (list size (rest dim-expr)) let-list)

             ;; Check that dimensions are consistent
             ;; Get list of dimension expressions

             (dolist (expr (cdr (remove-if-not (lambda (it) (eq it sym))
                                               dim-exprs :key #'car)))
               (push `(unless (= ,(rest expr) ,size)
                        (error "Incompatible sizes for index ~S : ~S and ~S" ',sym ,(rest dim-expr) ,(rest expr)))
                     checks-list)))

         ;; Wrap a loop around RESULT
           (setf result
                 `(loop for ,sym from 0 below ,size do
                       ,result)))

      `(let ,let-list
         ,@checks-list
         (let ((,result-array (make-array (list ,@(reverse index-sizes)) :element-type ,element-type )))
           ,result
           ,result-array)))))


(defmacro each-index! (array index &body body)
  "Sets elements of the given ARRAY to values of the BODY,
   evaluated at array indices INDEX

  Note: This has the same semantics as each-index and each-index*,
  but the INDEX ranges are taken from the ARRAY dimensions, not
  a code walker.
  "
  (let ((index (if (listp index) index
                   (list index)))  ; Ensure that INDEX is a list
        (result (gensym)))    ; If ARRAY is an expression, evaluate once only
    (dolist (sym index)
      (unless (symbolp sym) (error "~S is not a symbol" sym)))

    `(progn
       (let ((,result ,array))
         (nested-loop ,index (array-dimensions ,result)
           (setf (aref ,result ,@index) (progn ,@body)))
         ,result))))


(defmacro each-index (index &body body)
  "Given one or more symbols INDEX, walks the BODY expression
   to determine the index ranges by looking for
   AREF and ROW-MAJOR-AREF calls.

  Transpose of 2D array A

    (each-index (i j)
      (aref A j i))

  Diagonal of a square 2D array

    (each-index i (aref A i i))

  Turn a 2D array into an array of arrays

    (each-index i
      (each-index j
        (aref A i j)))

  Matrix-vector product:

    (each-index i
      (sum-index j
        (* (aref A i j) (aref x j))))

  "
  `(each-index* t ,index ,@body))




;;;

(defmacro sum-index (index &body body)
  "Sums over one or more INDEX symbols in an array expression.
   The range of these symbols is determined by walking the tree
   for AREF and ROW-MAJOR-AREF calls.

  Example:

   (defparameter A #2A((1 2) (3 4)))

   (sum-index i (row-major-aref A i))  ; Sum all elements
   => 10

   (sum-index (i j) (aref A i j))  ; Sum all elements
   => 10

   (sum-index i (aref A i i))  ; Trace of array
   => 5
  "
  (let ((dim-exprs (find-array-dimensions body))
        (index (if (listp index) index
                   (list index))))  ; Ensure that INDEX is a list
    ;; Check that all elements of INDEX are symbols
    (dolist (sym index)
      (unless (symbolp sym) (error "Index must be a symbol ~S" sym)))

    (let ((index-sizes (loop for i from 0 below (length index) collecting (gensym)))
          (let-list nil)   ; let environment
          (checks-list nil)  ; Array size checks
          (result (cons 'progn body)))   ; The result of this macro

      (loop for sym in (reverse index) for size in index-sizes do
         ;; Add a dimension to be set in the `let` environment
         ;; Find an expression which sets the range of SYM
           (let ((dim-expr (assoc sym dim-exprs)))
             (unless dim-expr (error "Cannot determine range of index ~S" sym))
             (push (list size (rest dim-expr)) let-list)
             ;; Check that dimensions are consistent
             ;; Get list of dimension expressions

             (dolist (expr (cdr (remove-if-not (lambda (it) (eq it sym))
                                               dim-exprs :key #'car)))
               (push `(unless (= ,(rest expr) ,size)
                        (error "Incompatible sizes for index ~S : ~S and ~S" ',sym ,(rest dim-expr) ,(rest expr)))
                     checks-list)))

         ;; Wrap a loop around RESULT
           (setf result
                 `(loop for ,sym from 0 below ,size summing
                       ,result)))

      `(let ,let-list
         ,@checks-list
         ,result))))

(defmacro reduce-index (function index &body body)
  "Reduction over one or more INDEX symbols in an array expression.
   The range of these symbols is determined by walking the tree
   for AREF and ROW-MAJOR-AREF calls.

  Example:

   (defparameter A #2A((1 2) (3 4)))

   (reduce-index #'+ i (row-major-aref A i))  ; Sum all elements (sum-index)
   => 10

   (reduce-index #'* (i j) (aref A i j))  ; Multiply all elements
   => 24

   (reduce-index #'max i (row-major-aref A i)) ; Maxmum value
   => 4
  "
  (let ((dim-exprs (find-array-dimensions body))
        (index (if (listp index) index
                   (list index))))  ; Ensure that INDEX is a list
    ;; Check that all elements of INDEX are symbols
    (dolist (sym index)
      (unless (symbolp sym) (error "Index must be a symbol ~S" sym)))

    (let ((index-sizes (loop for i from 0 below (length index) collecting (gensym)))
          (let-list nil)   ; let environment
          (checks-list nil))  ; Array size checks

      (loop for sym in (reverse index) for size in index-sizes do
         ;; Add a dimension to be set in the `let` environment
         ;; Find an expression which sets the range of SYM
           (let ((dim-expr (assoc sym dim-exprs)))
             (unless dim-expr (error "Cannot determine range of index ~S" sym))
             (push (list size (rest dim-expr)) let-list)
             ;; Check that dimensions are consistent
             ;; Get list of dimension expressions

             (dolist (expr (cdr (remove-if-not (lambda (it) (eq it sym))
                                               dim-exprs :key #'car)))
               (push `(unless (= ,(rest expr) ,size)
                        (error "Incompatible sizes for index ~S : ~S and ~S" ',sym ,(rest dim-expr) ,(rest expr)))
                     checks-list))))

      (let ((gnext (gensym))  ; Tagbody symbols
            (grun (gensym))
            (gdone (gensym))
            (result (gensym))) ; Reduced result

        `(let ,let-list  ; Get dimension sizes
           ,@checks-list ; Check that they are consistent

           (let ,(loop for sym in index collecting (list sym 0)) ; All indices 0
             (let ((,result (progn ,@body))) ; Evaluate expression as starting result
               (tagbody
                  ,gnext
                  ;; Increment indices, starting with innermost.
                  ,@(loop for sym in (reverse index) for size in index-sizes
                       append `((when (< (incf ,sym) ,size)  ; While in range
                                  (go ,grun))
                                (setf ,sym 0))) ; Reset index, fall through to increment next index
                  (go ,gdone) ; Reached the end, no more indices to increment
                  ,grun
                  ;; Perform reduction
                  (setf ,result (funcall ,function ,result (progn ,@body)))
                  (go ,gnext)
                  ,gdone)
               ,result)))))))

