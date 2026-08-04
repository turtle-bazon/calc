(in-package #:calc)

;;; Stack primitives

(defun apply-unary-op (op stack)
  (if stack
      (cons (funcall op (car stack)) (cdr stack))
      stack))

(defun apply-binary-op (op stack)
  (if (>= (length stack) 2)
      (cons (funcall op (second stack) (car stack)) (cddr stack))
      stack))

;;; Token classifiers

(defun is-unary-func (s)
  (member (string-upcase s) '("SIN" "COS" "TAN" "ASIN" "ACOS" "ATAN"
                              "LOG" "LOG10" "EXP" "SQRT" "ABS" "NEG"
                              "ROUND" "FLOOR" "CEIL" "NOT" "BITNOT"
                              "HEX" "BIN" "DEC")
          :test #'string=))

(defun is-binary-op (s)
  (member (string-upcase s) '("+" "-" "*" "/" "^" "MOD" "MIN" "MAX"
                              "GCD" "LCM" "LOGAND" "LOGIOR" "LOGXOR"
                              "AND" "OR" "SHL" "SHR")
          :test #'string=))

(defun is-comparison (s)
  (member (string-upcase s) '(">" "<" "=" ">=" "<=" "!=") :test #'string=))

(defun is-memory-op (s)
  (member (string-upcase s) '("M+" "M-" "MR" "MC")
          :test #'string=))

(defun is-stack-op (s)
  (member (string-upcase s) '("DUP" "SWAP" "DROP" "OVER" "ROT" "NIP"
                              "CLEAR" "DEPTH" "PICK" "TUCK")
          :test #'string=))

(defun is-control-op (s)
  (member (string-upcase s) '("?" "FOR" "IF" "THEN" "ELSE" "ENDIF"
                              "WHILE" "REPEAT" "UNTIL")
          :test #'string=))

;;; Memory

(defvar *memory* 0
  "Calculator memory register.")

(defun handle-memory (tok stack)
  (let ((u (string-upcase tok))
        (val (when stack (pop stack))))
    (cond
      ((string= u "M+") (when (numberp val) (incf *memory* val)))
      ((string= u "M-") (when (numberp val) (decf *memory* val)))
      ((string= u "MR") (push *memory* stack))
      ((string= u "MC") (setf *memory* 0)))
    stack))

;;; Stack operations

(defun handle-stack (tok stack)
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "DUP")
       (if stack (cons (car stack) stack) stack))
      ((string= u "SWAP")
       (if (>= (length stack) 2)
           (cons (second stack) (cons (car stack) (cddr stack)))
           stack))
      ((string= u "DROP")
       (if stack (cdr stack) stack))
      ((string= u "OVER")
       (if (>= (length stack) 2)
           (cons (second stack) stack)
           stack))
      ((string= u "ROT")
       (if (>= (length stack) 3)
           (cons (third stack)
                 (cons (car stack)
                       (cons (second stack) (cdddr stack))))
           stack))
      ((string= u "NIP")
       (if (>= (length stack) 2)
           (cons (car stack) (cddr stack))
           stack))
      ((string= u "CLEAR")
       nil)
      ((string= u "DEPTH")
       (cons (length stack) stack))
      ((string= u "PICK")
       (if (and stack (numberp (car stack)))
           (let ((n (pop stack)))
             (if (< n (length stack))
                 (cons (nth (- (length stack) 1 n) stack) stack)
                 stack))
           stack))
      ((string= u "TUCK")
       (if (>= (length stack) 2)
           (cons (car stack)
                 (cons (second stack)
                       (cons (car stack) (cddr stack))))
           stack))
      (t stack))))

;;; Function constructors

(defun make-binary-func (tok)
  (cond
    ((string= tok "+") #'+)
    ((string= tok "-") #'-)
    ((string= tok "*") #'*)
    ((string= tok "/") #'/)
    ((string= tok "^") #'expt)
    ((string-equal tok "MOD") #'mod)
    ((string-equal tok "MIN") #'min)
    ((string-equal tok "MAX") #'max)
    ((string-equal tok "GCD") #'gcd)
    ((string-equal tok "LCM") #'lcm)
    ((string-equal tok "LOGAND") #'logand)
    ((string-equal tok "LOGIOR") #'logior)
    ((string-equal tok "LOGXOR") #'logxor)
    ((string-equal tok "AND") (lambda (a b) (and a b)))
    ((string-equal tok "OR") (lambda (a b) (or a b)))
    ((string-equal tok "SHL") (lambda (a b) (ash (truncate a) b)))
    ((string-equal tok "SHR") (lambda (a b) (ash (truncate a) (- b))))))

(defun make-comparison-func (tok)
  (cond
    ((string= tok ">") (lambda (a b) (> a b)))
    ((string= tok "<") (lambda (a b) (< a b)))
    ((string= tok "=") (lambda (a b) (= a b)))
    ((string-equal tok ">=") (lambda (a b) (>= a b)))
    ((string-equal tok "<=") (lambda (a b) (<= a b)))
    ((string-equal tok "!=") (lambda (a b) (not (= a b))))))

(defun make-unary-func (tok)
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "SIN") #'sin)
      ((string= u "COS") #'cos)
      ((string= u "TAN") #'tan)
      ((string= u "ASIN") #'asin)
      ((string= u "ACOS") #'acos)
      ((string= u "ATAN") #'atan)
      ((string= u "LOG") #'log)
      ((string= u "LOG10") (lambda (x) (log x 10)))
      ((string= u "EXP") #'exp)
      ((string= u "SQRT") #'sqrt)
      ((string= u "ABS") #'abs)
      ((string= u "NEG") #'-)
      ((string= u "ROUND") #'round)
      ((string= u "FLOOR") #'floor)
      ((string= u "CEIL") #'ceiling)
      ((string= u "NOT") (lambda (x) (not x)))
      ((string= u "BITNOT") (lambda (x) (lognot (truncate x))))
      ((string= u "HEX") (lambda (x) (format nil "~X" (truncate x))))
      ((string= u "BIN") (lambda (x) (format nil "~B" (truncate x))))
      ((string= u "DEC") (lambda (x)
                           (if (stringp x)
                               (parse-integer x :junk-allowed t)
                               x))))))

;;; Factorial

(defun factorial (n)
  (if (and (integerp n) (>= n 0))
      (apply #'* (loop for i from 1 to n collect i))
      0))

;;; Control flow

(defun handle-ternary (stack)
  (if (>= (length stack) 3)
      (let ((false-val (pop stack))
            (true-val (pop stack))
            (condition (pop stack)))
        (cons (if condition true-val false-val) stack))
      stack))

(defun handle-for (stack vars funcs)
  (if (>= (length stack) 3)
      (let ((func-name (string-upcase (pop stack)))
            (end-val (pop stack))
            (start-val (pop stack)))
        (let ((func (gethash func-name funcs)))
          (when func
            (let ((arg-names (getf func :args))
                  (body (getf func :body)))
              (loop for i from start-val to end-val do
                (when arg-names
                  (setf (gethash (first arg-names) vars) i))
                (eval-rpn body vars funcs)))))
        stack)
      stack))

(defun handle-control (tok stack vars funcs)
  (cond
    ((string= tok "?")        (handle-ternary stack))
    ((string-equal tok "FOR") (handle-for stack vars funcs))
    (t stack)))

;;; Token dispatch

(defun dispatch-token (tok stack vars funcs)
  (cond
    ((is-binary-op tok)
     (apply-binary-op (make-binary-func tok) stack))
    ((is-comparison tok)
     (apply-binary-op (make-comparison-func tok) stack))
    ((is-unary-func tok)
     (apply-unary-op (make-unary-func tok) stack))
    ((string= tok "!")
     (apply-unary-op #'factorial stack))
    ((is-memory-op tok)
     (handle-memory tok stack))
    ((is-stack-op tok)
     (handle-stack tok stack))
    ((is-control-op tok)
     (handle-control tok stack vars funcs))
    ((or (string= tok "(") (string= tok ")"))
     stack)
    (t
     (cons (resolve-token tok vars) stack))))

;;; Main evaluator

(defun eval-rpn (expr vars funcs)
  (let ((stack nil))
    (dolist (tok (tokenize expr))
      (setf stack (dispatch-token tok stack vars funcs)))
    (if stack (car stack) nil)))
