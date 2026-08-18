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
    (is (= (calc:eval-rpn "0 !" vars funcs) 1))
    (signals calc:calc-error (calc:eval-rpn "-1 !" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "3.5 !" vars funcs))))

(test eval-ternary
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 10 20 ?" vars funcs) 10))
    (is (= (calc:eval-rpn "0 10 20 ?" vars funcs) 20))))

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

(test state-isolation
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    ;; Define a macro
    (setf (gethash "TESTMACRO" calc:*macros*) (list :args '("x") :body "x x +"))
    (is (= (calc:eval-rpn "5 testmacro" vars funcs) 10))
    ;; Simulate new session by resetting macros
    (setf calc:*macros* (make-hash-table :test #'equal))
    ;; Macro should not be available (returns nil)
    (is (null (calc:eval-rpn "5 testmacro" vars funcs)))))

;;; FOR loop tests

(test eval-for-loop
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 5 FOR I NEXT" vars funcs) 5))
    (is (= (calc:eval-rpn "0 10 FOR I NEXT" vars funcs) 10))))

(test eval-for-loop-with-body
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 5 FOR I 2 * NEXT" vars funcs) 10))))

;;; Extended math tests

(test eval-hypot
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "3 4 hypot" vars funcs) 5))))(test eval-square-cube-cubert
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 square" vars funcs) 25))
    (is (= (calc:eval-rpn "3 cube" vars funcs) 27))
    (is (< (abs (- (calc:eval-rpn "8 cubert" vars funcs) 2.0)) 0.001))
    (signals calc:calc-error (calc:eval-rpn "-1 cubert" vars funcs))))

(test map-array
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equalp '(0 -1 -2 -3) (calc:eval-rpn "[ 0 1 2 3 ] \"NEG\" MAP" vars funcs)))))

(test filter-array
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equalp '(0 1 2 3) (calc:eval-rpn "[ 0 1 2 3 ] \"ABS\" FILTER" vars funcs)))
    (is (equalp '(1 2 3) (calc:eval-rpn "[ 1 2 3 ] \"ABS\" FILTER" vars funcs)))))

(test reduce-array
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= 10 (calc:eval-rpn "[ 1 2 3 4 ] \"+\" 0 REDUCE" vars funcs)))
    (is (= 24 (calc:eval-rpn "[ 1 2 3 4 ] \"*\" 1 REDUCE" vars funcs)))))
(test eval-atan2
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "1 1 atan2" vars funcs) (/ pi 4))) 0.001))))

(test eval-extended-bitwise
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 3 logeqv" vars funcs) -7))
    (is (eq (calc:eval-rpn "5 3 xor" vars funcs) t))
    (is (eq (calc:eval-rpn "5 5 nand" vars funcs) nil))
    (is (eq (calc:eval-rpn "5 5 nor" vars funcs) nil))))

(test eval-min-max3
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "3 1 2 min3" vars funcs) 1))
    (is (= (calc:eval-rpn "3 1 2 max3" vars funcs) 3))
    (is (= (calc:eval-rpn "5 1 10 clamp" vars funcs) 5))
    (is (= (calc:eval-rpn "0 1 10 clamp" vars funcs) 1))
    (is (= (calc:eval-rpn "15 1 10 clamp" vars funcs) 10))))

;;; Array tests

(test eval-array-literal
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ 1 2 3 ]" vars funcs) '(1 2 3)))
    (is (equal (calc:eval-rpn "[ ]" vars funcs) nil))))

(test eval-array-ops
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "[ 1 2 3 ] 0 get" vars funcs) 1))
    (is (= (calc:eval-rpn "[ 1 2 3 ] 2 get" vars funcs) 3))
    (is (equal (calc:eval-rpn "[ 1 2 3 ] 1 99 set" vars funcs) '(1 99 3)))
    (is (= (calc:eval-rpn "[ 1 2 3 ] len" vars funcs) 3))
    (is (equal (calc:eval-rpn "[ 1 2 ] 3 push" vars funcs) '(1 2 3)))
    ;; POP returns the popped value, not the array
    (is (= (calc:eval-rpn "[ 1 2 3 ] pop" vars funcs) 3))))

(test eval-array-append
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ 1 2 ] [ 3 4 ] append" vars funcs) '(1 2 3 4)))))

;;; String tests

(test eval-string-literal
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (string= (calc:eval-rpn "\"hello\"" vars funcs) "hello"))
    (is (string= (calc:eval-rpn "\"\"" vars funcs) ""))))

(test eval-string-ops
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "\"hello\" strlen" vars funcs) 5))
    (is (string= (calc:eval-rpn "\"hello\" \" world\" strcat" vars funcs) "hello world"))
    (is (string= (calc:eval-rpn "\"hello\" 1 3 substr" vars funcs) "ell"))
    (is (string= (calc:eval-rpn "\"hello\" upper" vars funcs) "HELLO"))
    (is (string= (calc:eval-rpn "\"HELLO\" lower" vars funcs) "hello"))
    (is (string= (calc:eval-rpn "\"  hello  \" trim" vars funcs) "hello"))))

;;; Macro tests

(test eval-macro
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (setf (gethash "DOUBLE" calc:*macros*) (list :args '("x") :body "x x +"))
    (is (= (calc:eval-rpn "5 double" vars funcs) 10))
    (setf (gethash "SQUARE" calc:*macros*) (list :args '("x") :body "x x *"))
    (is (= (calc:eval-rpn "4 square" vars funcs) 16))))

;;; Nested array tests

(test eval-nested-arrays
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ [ 1 2 ] [ 3 4 ] ]" vars funcs) '((1 2) (3 4))))
    (is (equal (calc:eval-rpn "[ [ 1 ] 2 3 ]" vars funcs) '((1) 2 3)))
    (is (= (calc:eval-rpn "[ [ 1 2 ] ] 0 get 0 get" vars funcs) 1))))

;;; Lambda tests

(test eval-lambda
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "( x ) x x + 5 call" vars funcs) 10))
    (is (= (calc:eval-rpn "( a b ) a b * 3 4 call" vars funcs) 12))))

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

;;; Error handling tests

(test error-division-by-zero
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "1 0 /" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "1 0 mod" vars funcs))))

(test error-log-negative
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "-1 log" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "-1 log10" vars funcs))))

(test error-sqrt-negative
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "-1 sqrt" vars funcs))))

(test error-stack-underflow
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "+" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "sin" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "!" vars funcs))))

(test error-pick-out-of-range
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "1 5 pick" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "1 -1 pick" vars funcs))))

(test error-memory-empty-stack
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (setf calc:*memory* 0)
    (signals calc:calc-error (calc:eval-rpn "M+" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "M-" vars funcs))))

(test error-stack-ops-underflow
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (signals calc:calc-error (calc:eval-rpn "dup" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "swap" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "drop" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "over" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "rot" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "nip" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "tuck" vars funcs))))



(defun run-tests ()
  (let ((results (fiveam:run 'calc-suite)))
    (fiveam:explain! results)
    (fiveam:results-status results)))
(test eval-hyperbolic-trig
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (calc:eval-rpn "0 sinh" vars funcs)) 0.001))
    (is (< (abs (- (calc:eval-rpn "0 cosh" vars funcs) 1)) 0.001))
    (is (< (abs (calc:eval-rpn "0 tanh" vars funcs)) 0.001))
    (is (< (abs (calc:eval-rpn "0 asinh" vars funcs)) 0.001))
    (is (< (abs (calc:eval-rpn "1 acosh" vars funcs)) 0.001))
    (is (< (abs (calc:eval-rpn "0 atanh" vars funcs)) 0.001))))(test eval-stats
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "[ 1 2 3 4 5 ] mean" vars funcs) 3))
    (is (= (calc:eval-rpn "[ 1 2 3 4 5 ] median" vars funcs) 3))
    (is (< (abs (- (calc:eval-rpn "[ 2 4 4 4 5 5 7 9 ] stddev" vars funcs) 2.0)) 0.01))))(test eval-rand (let ((vars (make-hash-table :test (quote equal))) (funcs (make-hash-table :test (quote equal)))) (is (numberp (calc:eval-rpn "1 RAND" vars funcs))) (is (integerp (calc:eval-rpn "10 RANDINT" vars funcs))) (is (< 0 (calc:eval-rpn "10 RANDINT" vars funcs) 10))))(test eval-constants (let ((vars (make-hash-table :test (quote equal))) (funcs (make-hash-table :test (quote equal)))) (is (< (abs (- (calc:eval-rpn "PI" vars funcs) 3.141592653589793d0)) 0.0001)) (is (< (abs (- (calc:eval-rpn "E" vars funcs) 2.718281828459045d0)) 0.0001))))(test eval-signum
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 SIGNUM" vars funcs) 1))
    (is (= (calc:eval-rpn "0 SIGNUM" vars funcs) 0))
    (is (= (calc:eval-rpn "-5 SIGNUM" vars funcs) -1))))

(test eval-idiv
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "10 3 IDIV" vars funcs) 3))
    (is (= (calc:eval-rpn "7 2 IDIV" vars funcs) 3))
    (is (= (calc:eval-rpn "100 10 IDIV" vars funcs) 10))
    (is (= (calc:eval-rpn "-7 2 IDIV" vars funcs) -3))))