;;;; package.lisp

(defpackage #:array-operations
  (:use #:cl
        #:alexandria
        #:optima)
  (:nicknames #:aops)
  (:shadow #:flatten)
  (:export ; creation
   #:fill!
   #:zeros
   #:zeros*
   #:zeros!
   #:ones
   #:ones*
   #:ones!
   #:rand
   #:rand*
   #:rand!
   #:randn
   #:randn*
   #:randn!
   #:linspace
   #:linspace*
   #:linspace!
   #:similar-array)
  (:export ; utilities
   #:walk-subscripts
   #:walk-subscripts-list
   #:nested-loop)
  (:export ; general
   #:as-array
   #:element-type
   #:dims
   #:size
   #:rank
   #:dim
   #:&dims
   #:nrow
   #:ncol
   #:array-matrix
   #:matrix?
   #:square-matrix?
   #:make-array-like)
  (:export ; displacement
   #:displace
   #:flatten
   #:split
   #:copy-into
   #:sub
   #:partition
   #:combine
   #:subvec
   #:reshape
   #:reshape-col
   #:reshape-row)
  (:export ; transformations
   #:coercing
   #:generate*
   #:generate
   #:permutation-repeated-index
   #:permutation-invalid-index
   #:permutation-incompatible-rank
   #:valid-permutation?
   #:complement-permutation
   #:complete-permutation
   #:invert-permutation
   #:identity-permutation?
   #:permute
   #:each*
   #:each
   #:margin*
   #:margin
   #:recycle
   #:outer*
   #:outer
   #:vectorize!
   #:vectorize*
   #:vectorize)
  (:export ; reductions
   #:argmax
   #:argmin
   #:vectorize-reduce)
  (:export ; indexing
   #:each-index
   #:each-index*
   #:each-index!
   #:sum-index
   #:reduce-index)
  (:export ; stack
   #:copy-row-major-block
   #:stack-rows-copy
   #:stack-rows*
   #:stack-rows
   #:stack-cols-copy
   #:stack-cols*
   #:stack-cols
   #:stack*
   #:stack))
