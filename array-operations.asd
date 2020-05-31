(asdf:defsystem :array-operations
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src/"
  :depends-on (#:let-plus
               :array-operations/all)
  :in-order-to ((test-op (test-op :array-operations/tests))))

(asdf:defsystem :array-operations/tests
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (:array-operations       ; loads everything else
               :alexandria
               :clunit2)
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :array-operations/tests :run)))
