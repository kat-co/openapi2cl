LISP=sbcl --non-interactive

.PHONY: test
test:
	$(LISP) --eval "(asdf:test-system :openapi2cl)"

% : %.asd
	$(LISP) --eval "(asdf:make :$@)"
