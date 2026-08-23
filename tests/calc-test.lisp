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

(test eval-npr
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 3 NPR" vars funcs) 60))
    (is (= (calc:eval-rpn "10 0 NPR" vars funcs) 1))
    (is (= (calc:eval-rpn "5 5 NPR" vars funcs) 120))))

(test eval-ncr
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "5 3 NCR" vars funcs) 10))
    (is (= (calc:eval-rpn "10 0 NCR" vars funcs) 1))
    (is (= (calc:eval-rpn "5 5 NCR" vars funcs) 1))))

(test eval-sum-count
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "[ 1 2 3 4 5 ] sum" vars funcs) 15))
    (is (= (calc:eval-rpn "[ 1 2 3 4 5 ] count" vars funcs) 5))
    (is (= (calc:eval-rpn "[ 10 ] sum" vars funcs) 10))
    (is (= (calc:eval-rpn "[ 10 ] count" vars funcs) 1))))

(test eval-constants-extra
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "SQRT2" vars funcs) (sqrt 2))) 0.0001))
    (is (< (abs (- (calc:eval-rpn "LN2" vars funcs) (log 2))) 0.0001))))
(test process-defun (let ((vars (make-hash-table :test 'equal)) (funcs (make-hash-table :test 'equal))) (calc:process-expression "defun double(x) x 2 *" vars funcs) (let ((func (gethash "DOUBLE" funcs))) (is (not (null func))) (is (equal (getf func :args) '("x"))) (is (string= (getf func :body) " x 2 *")))))
(test eval-if-else (let ((vars (make-hash-table :test 'equal)) (funcs (make-hash-table :test 'equal))) (is (= (calc:eval-rpn "1 if 10 else 20 then" vars funcs) 10)) (is (= (calc:eval-rpn "0 if 10 else 20 then" vars funcs) 20))))
(test eval-golden (let ((vars (make-hash-table :test 'equal)) (funcs (make-hash-table :test 'equal))) (is (< (abs (- (calc:eval-rpn "GOLDEN" vars funcs) (/ (+ 1 (sqrt 5)) 2))) 0.0001))))
(test eval-stats-edge-cases (let ((vars (make-hash-table :test 'equal)) (funcs (make-hash-table :test 'equal))) (is (= (calc:eval-rpn "[ 5 ] mean" vars funcs) 5)) (is (= (calc:eval-rpn "[ 5 ] median" vars funcs) 5)) (is (= (calc:eval-rpn "[ 3 7 ] mean" vars funcs) 5)) (is (= (calc:eval-rpn "[ 3 7 ] median" vars funcs) 5))))
(test eval-semicolon (let ((vars (make-hash-table :test #'equal)) (funcs (make-hash-table :test #'equal))) (dolist (expr '("a = 1 2 +" "b = 3 4 +")) (calc:process-expression expr vars funcs)) (is (= (gethash "a" vars) 3)) (is (= (gethash "b" vars) 7))))
(test eval-while-false
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (null (calc:eval-rpn "begin 0 while 5 repeat" vars funcs)))))

(test eval-until-true
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "1 begin 1 until" vars funcs) 1))
    (is (= (calc:eval-rpn "0 begin 1 until" vars funcs) 0))))
(test eval-if-else-nested
  (let ((vars (make-hash-table :test (quote equal)))
        (funcs (make-hash-table :test (quote equal))))
    (is (= (calc:eval-rpn "1 if 1 if 10 else 20 then else 30 then" vars funcs) 10))
    (is (= (calc:eval-rpn "1 if 0 if 10 else 20 then else 30 then" vars funcs) 20))
    (is (= (calc:eval-rpn "0 if 1 if 10 else 20 then else 30 then" vars funcs) 30))))
(test eval-not
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (null (calc:eval-rpn "1 not" vars funcs)))
    (is (eq (calc:eval-rpn "0 not" vars funcs) t))
    (is (equal (calc:eval-rpn "[ 0 5 0 ] \"not\" MAP" vars funcs) '(t nil t)))))
(test eval-string-comparison
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (eq (calc:eval-rpn "\"abc\" \"abc\" =" vars funcs) t))
    (is (null (calc:eval-rpn "\"abc\" \"abd\" =" vars funcs)))
    (is (eq (calc:eval-rpn "\"abc\" \"abd\" <" vars funcs) t))
    (is (null (calc:eval-rpn "\"abd\" \"abc\" <" vars funcs)))
    (is (eq (calc:eval-rpn "\"abd\" \"abc\" >" vars funcs) t))
    (is (eq (calc:eval-rpn "\"abc\" \"abc\" >=" vars funcs) t))
    (is (eq (calc:eval-rpn "\"abc\" \"abd\" !=" vars funcs) t))
    (is (eq (calc:eval-rpn "3 5 <" vars funcs) t))))
(test eval-for-loop
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "0 1 5 FOR I + NEXT" vars funcs) 15))
    (is (= (calc:eval-rpn "1 1 3 FOR I * NEXT" vars funcs) 6))))

(test eval-lambda-multi-param
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "( x y ) x y * 3 4 call" vars funcs) 12))
    (is (= (calc:eval-rpn "( x y z ) x y + z * 1 2 3 call" vars funcs) 9))))

(test eval-error-messages
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (string= (handler-case (calc:eval-rpn "1 +" vars funcs)
                   (calc:calc-error (e) (calc:calc-error-message e)))
                 "Stack underflow: not enough arguments"))
    (is (string= (handler-case (calc:eval-rpn "1 0 /" vars funcs)
                   (calc:calc-error (e) (calc:calc-error-message e)))
                 "Division by zero"))
    (is (string= (handler-case (calc:eval-rpn "1 if 10" vars funcs)
                   (calc:calc-error (e) (calc:calc-error-message e)))
                 "IF without matching THEN"))))
(test eval-array-comparison
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (eq (calc:eval-rpn "[ 1 2 ] [ 1 2 ] =" vars funcs) t))
    (is (null (calc:eval-rpn "[ 1 2 ] [ 1 3 ] =" vars funcs)))
    (is (eq (calc:eval-rpn "[ 1 2 ] [ 1 3 ] !=" vars funcs) t))
    (is (eq (calc:eval-rpn "[ 1 ] [ 1 2 ] <" vars funcs) t))
    (is (eq (calc:eval-rpn "[ 1 2 ] [ 1 ] >" vars funcs) t))))

(test eval-array-min-max-sort-reverse
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "[ 3 1 2 ] AMIN" vars funcs) 1))
    (is (= (calc:eval-rpn "[ 3 1 2 ] AMAX" vars funcs) 3))
    (is (equal (calc:eval-rpn "[ 3 1 2 ] SORT" vars funcs) '(1 2 3)))
    (is (equal (calc:eval-rpn "[ 1 2 3 ] REVERSE" vars funcs) '(3 2 1)))))

(test eval-string-repeat-concat
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (string= (calc:eval-rpn "\"ab\" 3 *" vars funcs) "ababab"))
    (is (string= (calc:eval-rpn "3 \"ab\" *" vars funcs) "ababab"))
    (is (string= (calc:eval-rpn "\"\" 5 *" vars funcs) ""))
    (is (string= (calc:eval-rpn "\"ab\" \"cd\" +" vars funcs) "abcd"))
    (is (= (calc:eval-rpn "6 7 *" vars funcs) 42))))

(test eval-in-operator
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (eq (calc:eval-rpn "3 [ 1 2 3 ] IN" vars funcs) t))
    (is (null (calc:eval-rpn "9 [ 1 2 3 ] IN" vars funcs)))
    (is (eq (calc:eval-rpn "\"b\" [ \"a\" \"b\" ] IN" vars funcs) t))))
(test eval-slice-index
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ 1 2 3 4 ] 1 2 SLICE" vars funcs) '(2 3)))
    (is (equal (calc:eval-rpn "[ 1 2 3 ] 0 5 SLICE" vars funcs) '(1 2 3)))
    (is (= (calc:eval-rpn "6 [ 5 6 7 ] INDEX" vars funcs) 1))
    (is (= (calc:eval-rpn "9 [ 5 6 7 ] INDEX" vars funcs) -1))))

(test eval-variance-range-mode
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "[ 2 4 4 4 5 5 7 9 ] VARIANCE" vars funcs) 4))
    (is (= (calc:eval-rpn "[ 3 1 9 2 ] RANGE" vars funcs) 8))
    (is (= (calc:eval-rpn "[ 1 2 2 3 2 ] MODE" vars funcs) 2))
    (is (= (calc:eval-rpn "[ 7 ] MODE" vars funcs) 7))))

(test eval-degree-trig
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "30 SIND" vars funcs) 0.5)) 0.0001))
    (is (< (abs (- (calc:eval-rpn "60 COSD" vars funcs) 0.5)) 0.0001))
    (is (< (abs (- (calc:eval-rpn "45 TAND" vars funcs) 1)) 0.0001))))
(test process-defmacro-forms
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (calc:process-expression "defmacro sub2(a b) a b -" vars funcs)
    (is (not (null (gethash "SUB2" calc::*macros*))))
    (calc:process-expression "defmacro five 3 2 +" vars funcs)
    (is (not (null (gethash "FIVE" calc::*macros*))))
    (calc:process-expression "defmacro double(x) x 2 *" vars funcs)
    (is (not (null (gethash "DOUBLE" calc::*macros*))))))

(test eval-macro-calls
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (calc:process-expression "defmacro sub2(a b) a b -" vars funcs)
    (is (= (calc:eval-rpn "10 3 sub2" vars funcs) 7))
    (calc:process-expression "defmacro avg(a b) a b + 2 /" vars funcs)
    (is (= (calc:eval-rpn "4 10 avg" vars funcs) 7))
    (calc:process-expression "defmacro five 3 2 +" vars funcs)
    (is (= (calc:eval-rpn "five" vars funcs) 5))
    (calc:process-expression "defmacro double(x) x 2 *" vars funcs)
    (is (= (calc:eval-rpn "21 double" vars funcs) 42))))

(test eval-ternary-truthiness
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "\"a\" 10 20 ?" vars funcs) 10))
    (is (= (calc:eval-rpn "0 10 20 ?" vars funcs) 20))
    (is (= (calc:eval-rpn "5 10 20 ?" vars funcs) 10))
    (is (equal (calc:eval-rpn "[ 1 ] 10 20 ?" vars funcs) 10))))
(test eval-until-loop (let ((vars (make-hash-table :test 'equal)) (funcs (make-hash-table :test 'equal))) (is (= (calc:eval-rpn "1 begin 2 * dup 16 > until" vars funcs) 32))))


(test eval-defun-call
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (let ((out (with-output-to-string (*standard-output*)
                 (calc:process-expression "defun sq(x) x x *" vars funcs)
                 (calc:process-expression "sq(5)" vars funcs)
                 (calc:process-expression "defun add(a b) a b +" vars funcs)
                 (calc:process-expression "add(2,3)" vars funcs))))
      (is (search "= 25" out))
      (is (search "= 5" out))
      ;; hygienic binding: arguments must NOT leak into global vars
      (is (null (gethash "X" vars)))
      (is (null (gethash "A" vars)))
      (is (null (gethash "B" vars))))))

(test eval-for-named-var
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "0 1 4 FOR K DROP K NEXT" vars funcs) 4))))
(test eval-for-nested
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "0 1 3 FOR I 1 1 FOR J DROP NEXT I + NEXT" vars funcs) 6))
    (is (null (gethash "%FOR-STACK" vars)))
    (is (= (calc:eval-rpn "0 1 4 FOR K DROP K DUP * + NEXT" vars funcs) 30))
    (is (null (gethash "%FOR-STACK" vars)))))
(test eval-tau
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "TAU" vars funcs)
                   (* 2 pi))) 1e-10))
    (is (< (abs (- (calc:eval-rpn "0 TAU SIN" vars funcs) 0)) 1e-9))))

(test eval-nroot
(test eval-deg-rad
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (< (abs (- (calc:eval-rpn "PI DEG" vars funcs) 180)) 1e-9))
    (is (< (abs (- (calc:eval-rpn "180 RAD" vars funcs) pi)) 1e-9))
    (is (< (abs (- (calc:eval-rpn "30 RAD DEG" vars funcs) 30)) 1e-9))))

(test eval-reverse-string
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (string= (calc:eval-rpn "\"abc\" REVERSE" vars funcs) "cba"))
    (is (equal (calc:eval-rpn "[ 1 2 3 ] REVERSE" vars funcs) '(3 2 1)))
    (signals calc:calc-error (calc:eval-rpn "[ 1 \"a\" ] REVERSE" vars funcs))))

  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (= (calc:eval-rpn "8 3 NROOT" vars funcs) 2))
    (is (= (calc:eval-rpn "27 3 NROOT" vars funcs) 3))
    (is (= (calc:eval-rpn "16 2 NROOT" vars funcs) 4))
    (is (= (calc:eval-rpn "-8 3 NROOT" vars funcs) -2))
    (signals calc:calc-error (calc:eval-rpn "8 0 NROOT" vars funcs))
    (signals calc:calc-error (calc:eval-rpn "-8 2 NROOT" vars funcs))))
(test eval-string-array-ops
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ \"b\" \"a\" \"c\" ] SORT" vars funcs) '("a" "b" "c")))
    (is (string= (calc:eval-rpn "[ \"pear\" \"apple\" ] AMIN" vars funcs) "apple"))
    (is (string= (calc:eval-rpn "[ \"pear\" \"apple\" ] AMAX" vars funcs) "pear"))
    ;; numeric paths unchanged
    (is (equal (calc:eval-rpn "[ 3 1 2 ] SORT" vars funcs) '(1 2 3)))
    (is (= (calc:eval-rpn "[ 5 1 ] AMIN" vars funcs) 1))
    (is (= (calc:eval-rpn "[ 5 1 ] AMAX" vars funcs) 5))
    ;; mixed types are rejected
    (signals calc:calc-error (calc:eval-rpn "[ 1 \"a\" ] SORT" vars funcs))))
(test eval-in-substring
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (eq (calc:eval-rpn "\"b\" \"abc\" IN" vars funcs) t))
    (is (null (calc:eval-rpn "\"z\" \"abc\" IN" vars funcs)))
    (is (eq (calc:eval-rpn "3 [ 1 2 3 ] IN" vars funcs) t))))
(test eval-array-arithmetic
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (is (equal (calc:eval-rpn "[ 1 2 ] [ 3 4 ] +" vars funcs) '(4 6)))
    (is (equal (calc:eval-rpn "[ 1 2 ] [ 3 5 ] -" vars funcs) '(-2 -3)))
    (is (equal (calc:eval-rpn "[ 1 2 ] [ 3 4 ] *" vars funcs) '(3 8)))
    (is (equal (calc:eval-rpn "[ 8 4 ] [ 2 2 ] /" vars funcs) '(4 2)))
    ;; scalar broadcast, both sides
    (is (equal (calc:eval-rpn "[ 1 2 ] 10 *" vars funcs) '(10 20)))
    (is (equal (calc:eval-rpn "10 [ 2 4 ] /" vars funcs) '(5 5/2)))
    ;; length mismatch is an explicit error
    (signals calc:calc-error (calc:eval-rpn "[ 1 2 ] [ 3 ] +" vars funcs))
    ;; strings and plain numbers keep their old paths
    (is (string= (calc:eval-rpn "\"ab\" \"cd\" +" vars funcs) "abcd"))
    (is (= (calc:eval-rpn "6 7 *" vars funcs) 42))))
(test tokenize-edge-cases
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    ;; paren shapes: glued digits stay one token; spaced forms are lambda-shaped
    (is (equal (calc:tokenize "(1)") '("(1)")))
    (is (equal (calc:tokenize "( 1 )") '("( 1 )")))
    (is (equal (calc:tokenize "2(3)") '("2" "(3)")))
    ;; negatives: sign binds when no value precedes
    (is (equal (calc:tokenize "-5") '("-5")))
    (is (equal (calc:tokenize "- 5") '("-" "5")))
    ;; numbers
    (is (equal (calc:tokenize "3.14") '("3.14")))
    (is (equal (calc:tokenize "1e3") '("1e3")))
    (is (= (calc:eval-rpn "-5 2 +" vars funcs) -3))
    (is (= (calc:eval-rpn "1e3 2 +" vars funcs) 1002))
    ;; strings keep interior spaces; arrays nest as one token
    (is (equal (calc:tokenize "\"hi there\"") '("\"hi there\"")))
    (is (equal (calc:tokenize "[ [ 1 ] [ 2 ] ]") '("[ [ 1 ] [ 2 ] ]")))
    ;; operators split even when glued; whitespace runs collapse; case kept
    (is (equal (calc:tokenize "1+2") '("1" "+" "2")))
    (is (equal (calc:tokenize "  1   2  ") '("1" "2")))
    (is (equal (calc:tokenize "pi") '("pi")))
    (is (null (calc:tokenize "")))))
