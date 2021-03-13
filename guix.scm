(use-modules
 (guix packages)
 (guix build-system asdf)
 ((guix licenses) #:prefix license:)

 (gnu packages lisp-xyz)
 (gnu packages web))

(package
 (name "sbcl-openapi2cl")
 (version "0.0.1")
 (source (getcwd))
 (build-system asdf-build-system/sbcl)
 (inputs
  `(("cl-strings" ,sbcl-cl-strings)
    ("kebab" ,sbcl-kebab)
    ("yason" ,sbcl-yason)
    ("libyaml" ,libyaml)))
 (home-page "https://github.com/kat-co/openapi2cl")
 (synopsis "Generate a Common Lisp client from an Open API spec")
 (description
  "A library to generate a common-lisp client from OpenAPI files.")
 (license license:gpl3))
