(asdf:defsystem "calc"
  :description "Calculator Monster - RPN Calculator"
  :version "0.0.1.0"
  :license "GPL-3.0"
  :author "Calculator Monster Contributors"
  :depends-on ("uiop" "clingon")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "tokenizer")
     (:file "resolver")
     (:file "evaluator")
     (:file "processor")
     (:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/calc"
  :entry-point "calc:calc-toplevel")
