#-asdf3.1 (error "openapi2cl requires >= ASDF 3.1")

(asdf:defsystem :openapi2cl
  :description "A library to generate a common-lisp client from OpenAPI files."
  :author "Katherine Cox-Buday <cox.katherine.e@gmail.com>"
  :license  "GNU GPL v3"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("openapi2cl/core")
  :in-order-to ((test-op (test-op "openapi2cl/tests"))))

(asdf:defsystem :openapi2cl/tests
  :class :package-inferred-system
  :depends-on ("rove"
               "openapi2cl/tests/core")
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
