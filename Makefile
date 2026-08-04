LISP ?= sbcl

.PHONY: build test test-integration test-all clean

build:
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "calc")' \
	  --eval '(ensure-directories-exist #p"build/calc")' \
	  --eval '(asdf:make "calc")'

test:
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "calc-tests")' \
	  --eval '(unless (calc-tests:run-tests) (uiop:quit 1))'

test-integration: build
	tests/integration.sh

test-all: test test-integration

clean:
	rm -rf build
