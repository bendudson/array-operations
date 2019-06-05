[![Build Status](https://travis-ci.org/bendudson/array-operations.svg?branch=master)](https://travis-ci.org/bendudson/array-operations)

Introduction
============

This library is a collection of functions and macros for manipulating
Common Lisp arrays and performing numerical calculations with them. 

For example, arrays can be created:
```common-lisp
;; uniform and normal random numbers
(rand '(2 2)) ; => #2A((0.62944734 0.2709539) (0.81158376 0.6700171))

;; linear ranges
(linspace 1 10 7) ; => #(1 5/2 4 11/2 7 17/2 10)

;; Using a function, optionally given index position
(generate #'identity '(2 3) :position) ; => #2A((0 1 2) (3 4 5))
```

Arrays can be manipulated:
```common-lisp
(defparameter A #2A((1 2) (3 4)))
(defparameter B #2A((2 3) (4 5)))

;; split along any dimension
(split A 1)  ; => #(#(1 2) #(3 4))

;; stack along any dimension
(stack 1 A B) ; => #2A((1 2 2 3) (3 4 4 5))

;; element-wise function map
(each #'+ #(0 1 2) #(2 3 5)) ; => #(2 4 7)

;; element-wise expressions
(vectorize (A B) (* A (sqrt B))) ; => #2A((1.4142135 3.4641016) (6.0 8.944272))

;; index operations e.g. matrix-matrix multiply:
(each-index (i j)
  (sum-index k
    (* (aref A i k) (aref B k j)))) ; => #2A((10 13) (22 29))
```

Installation
============

This library is on QuickLisp:

```common-lisp
  (ql:quickload :array-operations)
```
To get the latest version, clone into your Quicklisp local project directory:

``` {.bash}
  $ git clone https://github.com/bendudson/array-operations.git ~/quicklisp/local-projects/
```
Then load as above with `(ql:quickload :array-operations)`.

To run the test suite (using [clunit](https://github.com/tgutu/clunit)):

```common-lisp
  (ql:quickload :array-operations-tests)
  (array-operations-tests:run)
```

Notes:

 - Requires ASDF version 3.1.6 (2015-10-18) or later, as it uses the `package-inferred-system` extension.
   A symptom of an older version of ASDF is an error when loading `array-operations`:
   `System "array-operations/all" not found`
   

A quick tour of the library
===========================

Shorthand for frequently used Common Lisp array functions
---------------------------------------------------------

The library defines the following short function names that are synomyms
for Common Lisp operations:

  array-operations   | Common Lisp
  ------------------ | -------------------------------
  size               | array-total-size
  rank               | array-rank
  dim                | array-dimension
  dims               | array-dimensions
  nrow               | *number of rows in matrix*
  ncol               | *number of columns in matrix*

The `array-operations` package has the nickname `aops`, so you can use,
for example, `(aops:size my-array)` without `use`'ing the package.

Displaced arrays for fun and profit
-----------------------------------

> displaced array n. an array which has no storage of its own, but which
> is instead indirected to the storage of another array, called its
> target, at a specified offset, in such a way that any attempt to
> access the displaced array implicitly references the target array.
> (CLHS Glossary)

Displaced arrays are one of the niftiest features of Common Lisp. When
an array is displaced to another array, it shares structure with (part
of) that array. The two arrays do not need to have the same dimensions,
in fact, the dimensions do not be related at all as long as the
displaced array fits inside the original one. The row-major index of the
former in the latter is called the *offset* of the displacement.

Displaced arrays are usually constructed using `make-array`, but this
library also provides `displace` for that purpose:

```common-lisp
  (defparameter *a* #2A((1 2 3) (4 5 6)))
  (aops:displace *a* 2 1) ; => #(2 3)
```

**`flatten`** displaces to a row-major array:

```common-lisp
  (aops:flatten *a*) ; => #(1 2 3 4 5 6)
```

The real fun starts with `split`, which splits off subarrays nested
within a given axis:

```common-lisp
  (aops:split *a* 1) ; => #(#(1 2 3) #(4 5 6))
  (defparameter *b* #3A(((0 1) (2 3))
                        ((4 5) (6 7))))
  (aops:split *b* 0) ; => #3A(((0 1) (2 3)) ((4 5) (6 7)))
  (aops:split *b* 1) ; => #(#2A((0 1) (2 3)) #2A((4 5) (6 7)))
  (aops:split *b* 2) ; => #2A((#(0 1) #(2 3)) (#(4 5) #(6 7)))
  (aops:split *b* 3) ; => #3A(((0 1) (2 3)) ((4 5) (6 7)))
```

Note how splitting at `0` and the rank of the array returns the array
itself.

Now consider `sub`, which returns a specific array, composed of the
elements that would start with given subscripts:

```common-lisp
  (aops:sub *b* 0) ; => #2A((0 1) (2 3))
  (aops:sub *b* 0 1) ; => #(2 3)
  (aops:sub *b* 0 1 0) ; => 2
```

There is also a `(setf sub)` function.

**`partition`** returns a consecutive chunk of an array separated along its
first subscript:

```common-lisp
  (aops:partition #2A((0 1)
                    (2 3)
                    (4 5)
                    (6 7)
                    (8 9))
                1 3) ; => #2A((2 3) (4 5))
```

and also has a `(setf partition)` pair.

**`combine`** is the opposite of `split`:

```common-lisp
  (aops:combine #(#(0 1) #(2 3))) ; => #2A((0 1) (2 3))
```

**`subvec`** returns a displaced subvector:

```common-lisp
  (aops:subvec #(0 1 2 3 4) 2 4) ; => #(2 3)
```

There is also a `(setf subvec)` function, which is like `(setf subseq)`
except for demanding matching lengths.

Finally, **`reshape`** can be used to displace arrays into a different
shape:

```common-lisp
  (aops:reshape *a* '(3 2)) ; => #2A((1 2) (3 4) (5 6))
```

You can use `t` for one of the dimensions, to be filled in
automatically:

```common-lisp
  (aops:reshape *b* '(1 t)) ; => #2A((0 1 2 3 4 5 6 7))
```

**`reshape-col`** and **`reshape-row`** reshape your array into a column or row
matrix, respectively.

Dimension specifications
------------------------

Functions in the library accept the following in place of dimensions:

-   a list of dimensions (as for `make-array`),
-   a positive integer, which is used as a single-element list,
-   another array, the dimensions of which are used.

The last one allows you to specify dimensions with other arrays. For
example, to reshape an array `a1` to look like `a2`, you can use

```common-lisp
  (aops:reshape a1 a2)
```

instead of the longer form

```common-lisp
  (aops:reshape a1 (aops:dims a2))
```

Array creation and transformations
----------------------------------

When the resulting element type cannot be inferred, functions that
create and transform arrays are provided in pairs: one of these will
allow you to specify the array-element-type of the result, while the
other assumes it is `t`. The former ends with a `*`, and the
`element-type` is always its first argument. I give examples for the
versions without `*`, use the other when you are optimizing your code
and you are sure you can constrain to a given element-type.

*Element traversal order of these functions is unspecified*. The
reason for this is that the library may use parallel code in the future,
so it is unsafe to rely on a particular element traversal order.

The following functions all make a new array, taking the dimensions as
input. The version ending in `*` also takes the array type as first
argument. There are also versions ending in `!` which do not make a
new array, but take an array as first argument, which is modified and returned. 

  Function   | Description
  ---------- | ------------------------------------------------------------------
  zeros      | Filled with zeros
  ones       | Filled with ones
  rand       | Filled with uniformly distrubuted random numbers between 0 and 1
  randn      | Normally distributed with mean 0 and standard deviation 1
  linspace   | Evenly spaced numbers in given range

For example:
```{.commonlisp}
  (aops:rand '(2 2))  ; => #2A((0.6686077 0.59425664) (0.7987722 0.6930506))

  (aops:rand* 'single-float '(2 2)) ; => #2A((0.39332366 0.5557821) (0.48831415 0.10924244))

  (let ((a (make-array '(2 2) :element-type 'double-float)))
    ;; Modify array A, filling with random numbers
    (aops:rand! a))  ; => #2A((0.6324615478515625d0 0.4636608362197876d0)
                              (0.4145939350128174d0 0.5124958753585815d0))
```

**`generate`** (and `generate*`) allow you to generate arrays using
functions.

```common-lisp
  (aops:generate (lambda () (random 10)) 3) ; => #(6 9 5)
  (aops:generate #'identity '(2 3) :position) ; => #2A((0 1 2) (3 4 5))
  (aops:generate #'identity '(2 2) :subscripts)
  ;; => #2A(((0 0) (0 1)) ((1 0) (1 1)))
  (aops:generate #'cons '(2 2) :position-and-subscripts)
  ;; => #2A(((0 0 0) (1 0 1)) ((2 1 0) (3 1 1)))
```

Depending on the last argument, the function will be called with the
(row-major) position, the subscripts, both, or no argument.

**`permute`** can permutate subscripts (you can also invert, complement, and
complete permutations, look at the docstring and the unit tests).
Transposing is a special case of permute:

```common-lisp
  (defparameter *a* #2A((1 2 3) (4 5 6)))
  (aops:permute '(0 1) *a*) ; => #2A((1 2 3) (4 5 6))
  (aops:permute '(1 0) *a*) ; => #2A((1 4) (2 5) (3 6))
```

**`each`** applies a function to its (array) arguments elementwise:

```common-lisp
  (aops:each #'+ #(0 1 2) #(2 3 5)) ; => #(2 4 7)
```

**`vectorize`** is a macro which performs elementwise operations

```common-lisp
  (defparameter a #(1 2 3 4))
  (aops:vectorize (a) (* 2 a)) ; => #(2 4 6 8)

  (defparameter b #(2 3 4 5))
  (aops:vectorize (a b) (* a (sin b))) ; => #(0.9092974 0.28224 -2.2704074 -3.8356972)
```

There is also a version `vectorize*` which takes a type argument for the
resulting array, and a version `vectorize!` which sets elements in a
given array.

The semantics of **`margin`** are more difficult to explain, so perhaps an
example will be more useful. Suppose that you want to calculate column
sums in a matrix. You could `permute` (transpose) the matrix, `split`
its subarrays at rank one (so you get a vector for each row), and apply
the function that calculates the sum. `margin` automates that for you:

```common-lisp
  (aops:margin (lambda (column)
               (reduce #'+ column))
             #2A((0 1)
                 (2 3)
                 (5 7)) 0) ; => #(7 11)
```

But the function is much more general than this: the arguments `inner`
and `outer` allow arbitrary permutations before splitting.

Finally, **`recycle`** allows you to recycle arrays along inner and outer
dimensions:

```common-lisp
  (aops:recycle #(2 3) :inner 2 :outer 4)
  ; => #3A(((2 2) (3 3)) ((2 2) (3 3)) ((2 2) (3 3)) ((2 2) (3 3)))
```

Indexing operations
-------------------

**`nested-loop`** is a simple macro which iterates over a set of indices
with a given range

```common-lisp
  (defparameter A #2A((1 2) (3 4)))

  (aops:nested-loop (i j) (array-dimensions A)
    (setf (aref A i j) (* 2 (aref A i j))))
  A ; => #2A((2 4) (6 8))

  (aops:nested-loop (i j) '(2 3)
    (format t "(~a ~a) " i j)) ; => (0 0) (0 1) (0 2) (1 0) (1 1) (1 2) 
```

**`sum-index`** is a macro which uses a code walker to determine the
dimension sizes, summing over the given index or indices

```common-lisp
  (defparameter A #2A((1 2) (3 4)))

  ;; Trace
  (aops:sum-index i (aref A i i)) ; => 5

  ;; Sum array
  (aops:sum-index (i j) (aref A i j)) ; => 10

  ;; Sum array
  (aops:sum-index i (row-major-aref A i)) ; => 10
```

The main use for `sum-index` is in combination with `each-index`.

**`each-index`** is a macro which creates an array and iterates over the
elements. Like `sum-index` it is given one or more index symbols, and
uses a code walker to find array dimensions.

```common-lisp
  (defparameter A #2A((1 2) (3 4)))
  (defparameter B #2A((5 6) (7 8)))

  ;; Transpose
  (aops:each-index (i j) (aref A j i)) ; => #2A((1 3) (2 4))

  ;; Sum columns
  (aops:each-index i
    (aops:sum-index j
      (aref A j i))) ; => #(4 6)

  ;; Matrix-matrix multiply
  (aops:each-index (i j)
     (aops:sum-index k
        (* (aref A i k) (aref B k j)))) ; => #2A((19 22) (43 50))
```

**`reduce-index`** is a more general version of `sum-index`, which
applies a reduction operation over one or more indices. 

```common-lisp
  (defparameter A #2A((1 2) (3 4)))
  
  ;; Sum all values in an array
  (aops:reduce-index #'+ i (row-major-aref A i)) ; => 10
  
  ;; Maximum value in each row
  (aops:each-index i
    (aops:reduce-index #'max j
      (aref A i j)))  ; => #(2 4)
```

Reductions
----------

Some reductions over array elements can be done using the CL `reduce`
function, together with `aops:flatten`, which returns a displaced
vector:

```common-lisp
  (defparameter a #2A((1 2) (3 4)))
  (reduce #'max (aops:flatten a)) ; => 4
```

**`argmax`** and **`argmin`** find the `row-major-aref` index where an array is
maximum or minimum. They both return two values: the first value is the
index; the second is the array value at that index.

```common-lisp
  (defparameter a #(1 2 5 4 2))
  (aops:argmax a) ; => 2 5
  (aops:argmin a) ; => 0 1
```

More complicated reductions can be done with **`vectorize-reduce`**,
for example the maximum absolute difference between arrays:

```common-lisp
  (defparameter a #2A((1 2) (3 4)))
  (defparameter b #2A((2 2) (1 3)))

  (aops:vectorize-reduce #'max (a b) (abs (- a b))) ; => 2
```

See also `reduce-index` above.

Scalars as 0-dimensional arrays
-------------------------------

Library functions treat non-array objects as if they were equivalent to
0-dimensional arrays: for example, `(aops:split array (rank array))`
returns an array that effectively equivalent (`eq`) to array. Another
example is `recycle`:

```common-lisp
  (aops:recycle 4 :inner '(2 2)) ; => #2A((4 4) (4 4))
```

Stacking
--------

You can also stack compatible arrays along any axis:

```common-lisp
  (defparameter *a1* #(0 1 2))
  (defparameter *a2* #(3 5 7))
  (aops:stack 0 *a1* *a2*) ; => #(0 1 2 3 5 7)
  (aops:stack 1
            (aops:reshape-col *a1*)
            (aops:reshape-col *a2*)) ; => #2A((0 3) (1 5) (2 7))

```

