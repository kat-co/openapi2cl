LISP ?= guix environment --pure -lguix.scm -- sbcl --non-interactive

.PHONY: test
test:
	$(LISP) --eval "(asdf:test-system :openapi2cl)"

% : %.asd
	$(LISP) --eval "(require :cffi)"\
		--eval '(pushnew (format nil "~a/lib/" (uiop:getenv "GUIX_ENVIRONMENT")) cffi:*foreign-library-directories*)'\
		--eval "(asdf:make :$@)"
