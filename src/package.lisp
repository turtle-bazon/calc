(defpackage #:calc
  (:use #:cl)
  (:export #:main
           #:calc-toplevel
           #:tokenize
           #:resolve-token
           #:eval-rpn
           #:process-expression
           #:*features-list*
           #:*memory*
           #:*macros*
           #:calc-error
           #:calc-error-message))

(in-package #:calc)

(defvar *macros* (make-hash-table :test #'equal)
  "Table of defined macros.")
