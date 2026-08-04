(defpackage #:calc-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:calc-tests)

(def-suite calc-suite
  :description "Calculator Monster test suite")

(in-suite calc-suite)

;;; Tokenizer tests

(test tokenize-basic
  (is (equal (calc:tokenize "1 2 3") '("1" "2" "3")))
  (is (equal (calc:tokenize "1+2") '("1" "+" "2")))
  (is (equal (calc:tokenize "1 + 2") '("1" "+" "2"))))

(test tokenize-operators
  (is (equal (calc:tokenize ">=") '(">=")))
  (is (equal (calc:tokenize "<=") '("<=")))
  (is (equal (calc:tokenize "!=") '("!=")))
  (is (equal (calc:tokenize "3>=2") '("3" ">=" "2"))))

(test tokenize-empty
  (is (equal (calc:tokenize "") nil))
  (is (equal (calc:tokenize "   ") nil)))

;;; Resolver tests

(test resolve-numbers
  (let ((vars (make-hash-table :test #'equal)))
    (is (= (calc:resolve-token "42" vars) 42))))

(test resolve-constants
  (let ((vars (make-hash-table :test #'equal)))
    (is (= (calc:resolve-token "PI" vars) pi))
    (is (> (calc:resolve-token "E" vars) 2))
    (is (< (calc:resolve-token "E" vars) 3))
    (is (eq (calc:resolve-token "TRUE" vars) t))
    (is (eq (calc:resolve-token "NIL" vars) nil))))

;;; Evaluator tests

(test eval-arithmetic
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "2 3 +" vars funcs) 5))
    (is (= (calc:eval-rpn "10 3 -" vars funcs) 7))
    (is (= (calc:eval-rpn "4 5 *" vars funcs) 20))
    (is (= (calc:eval-rpn "10 2 /" vars funcs) 5))
    (is (= (calc:eval-rpn "2 10 ^" vars funcs) 1024))))

(test eval-trig
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "0 sin" vars funcs) 0)) 1))
    (is (< (abs (- (calc:eval-rpn "0 cos" vars funcs) 1)) 1))))

(test eval-comparisons
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (eq (calc:eval-rpn "5 3 >" vars funcs) t))
    (is (eq (calc:eval-rpn "3 5 >" vars funcs) nil))
    (is (eq (calc:eval-rpn "3 3 =" vars funcs) t))
    (is (eq (calc:eval-rpn "3 5 <=" vars funcs) t))))

(test eval-stack-ops
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 dup" vars funcs) 5))
    (is (= (calc:eval-rpn "1 2 swap -" vars funcs) 1))
    (is (= (calc:eval-rpn "5 drop 3" vars funcs) 3))))

(test eval-new-stack-ops
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 2 3 depth" vars funcs) 3))
    (is (= (calc:eval-rpn "1 2 3 clear 42" vars funcs) 42))
    (is (= (calc:eval-rpn "10 20 0 pick" vars funcs) 10))
    (is (= (calc:eval-rpn "10 20 1 pick" vars funcs) 20))
    (is (= (calc:eval-rpn "1 2 tuck" vars funcs) 2))))

(test eval-factorial
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 !" vars funcs) 120))
    (is (= (calc:eval-rpn "0 !" vars funcs) 1))))

(test eval-ternary
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 10 20 ?" vars funcs) 10))
    (is (= (calc:eval-rpn "0 10 20 ?" vars funcs) 10))))

(test eval-variables
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (setf (gethash "X" vars) 10)
    (is (= (calc:eval-rpn "X 5 +" vars funcs) 15))))

(test eval-mod
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "10 3 mod" vars funcs) 1))))

(test eval-bitwise
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 3 logand" vars funcs) 1))
    (is (= (calc:eval-rpn "5 3 logior" vars funcs) 7))))

;;; Memory tests

(test eval-memory
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (setf calc:*memory* 0)
    (calc:eval-rpn "10 M+" vars funcs)
    (is (= calc:*memory* 10))
    (calc:eval-rpn "5 M+" vars funcs)
    (is (= calc:*memory* 15))
    (calc:eval-rpn "3 M-" vars funcs)
    (is (= calc:*memory* 12))
    (is (= (calc:eval-rpn "MR" vars funcs) 12))
    (calc:eval-rpn "MC" vars funcs)
    (is (= calc:*memory* 0))))

;;; Processor tests

(test process-assignment
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (calc:process-expression "X = 42" vars funcs)
    (is (= (gethash "X" vars) 42))))

(test process-expression-eval
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "2 3 +" vars funcs) 5))))

;;; Integration

(test process-multiple-expressions
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (calc:process-expression "A = 10" vars funcs)
    (calc:process-expression "B = 20" vars funcs)
    (is (= (gethash "A" vars) 10))
    (is (= (gethash "B" vars) 20))))

(defun run-tests ()
  (let ((results (fiveam:run 'calc-suite)))
    (fiveam:explain! results)
    (fiveam:results-status results)))
