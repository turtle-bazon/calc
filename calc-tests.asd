(asdf:defsystem "calc-tests"
  :description "Test suite for Calculator Monster"
  :version "0.0.1.0"
  :license "GPL-3.0"
  :author "Calculator Monster Contributors"
  :depends-on ("calc" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "calc-test"))))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :calc-tests :run-tests)
               (error "Some tests failed."))))
