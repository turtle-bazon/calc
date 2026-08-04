(in-package #:calc)

;;; Error condition

(define-condition calc-error (error)
  ((message :initarg :message :reader calc-error-message))
  (:report (lambda (c stream)
             (format stream "~a" (calc-error-message c)))))

;;; Stack primitives

(defun apply-unary-op (op stack)
  (unless stack
    (error 'calc-error :message "Stack underflow: not enough arguments"))
  (cons (funcall op (car stack)) (cdr stack)))

(defun apply-binary-op (op stack)
  (when (< (length stack) 2)
    (error 'calc-error :message "Stack underflow: not enough arguments"))
  (cons (funcall op (second stack) (car stack)) (cddr stack)))

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
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "M+")
       (unless stack
         (error 'calc-error :message "M+ requires a value on the stack"))
       (incf *memory* (car stack))
       (cdr stack))
      ((string= u "M-")
       (unless stack
         (error 'calc-error :message "M- requires a value on the stack"))
       (decf *memory* (car stack))
       (cdr stack))
      ((string= u "MR") (push *memory* stack))
      ((string= u "MC") (setf *memory* 0) stack)
      (t stack))))

;;; Stack operations

(defun handle-stack (tok stack)
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "DUP")
       (unless stack
         (error 'calc-error :message "DUP requires a value on the stack"))
       (cons (car stack) stack))
      ((string= u "SWAP")
       (when (< (length stack) 2)
         (error 'calc-error :message "SWAP requires two values on the stack"))
       (cons (second stack) (cons (car stack) (cddr stack))))
      ((string= u "DROP")
       (unless stack
         (error 'calc-error :message "DROP requires a value on the stack"))
       (cdr stack))
      ((string= u "OVER")
       (when (< (length stack) 2)
         (error 'calc-error :message "OVER requires two values on the stack"))
       (cons (second stack) stack))
      ((string= u "ROT")
       (when (< (length stack) 3)
         (error 'calc-error :message "ROT requires three values on the stack"))
       (cons (third stack)
             (cons (car stack)
                   (cons (second stack) (cdddr stack)))))
      ((string= u "NIP")
       (when (< (length stack) 2)
         (error 'calc-error :message "NIP requires two values on the stack"))
       (cons (car stack) (cddr stack)))
      ((string= u "CLEAR")
       nil)
      ((string= u "DEPTH")
       (cons (length stack) stack))
      ((string= u "PICK")
       (when (< (length stack) 1)
         (error 'calc-error :message "PICK requires an index on the stack"))
       (unless (numberp (car stack))
         (error 'calc-error :message "PICK index must be a number"))
       (let ((n (car stack)))
         (when (or (< n 0) (>= n (length stack))
                   (not (integerp n)))
           (error 'calc-error :message "PICK index out of range"))
         (cons (nth (- (length stack) 1 n) stack) (cdr stack))))
      ((string= u "TUCK")
       (when (< (length stack) 2)
         (error 'calc-error :message "TUCK requires two values on the stack"))
       (cons (car stack)
             (cons (second stack)
                   (cons (car stack) (cddr stack)))))
      (t stack))))

;;; Function constructors

(defun make-binary-func (tok)
  (cond
    ((string= tok "+") #'+)
    ((string= tok "-") #'-)
    ((string= tok "*") #'*)
    ((string= tok "/")
     (lambda (a b)
       (when (= b 0)
         (error 'calc-error :message "Division by zero"))
       (/ a b)))
    ((string= tok "^") #'expt)
    ((string-equal tok "MOD")
     (lambda (a b)
       (when (= b 0)
         (error 'calc-error :message "Division by zero (MOD)"))
       (mod a b)))
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
      ((string= u "LOG")
       (lambda (x)
         (when (<= x 0)
           (error 'calc-error :message "LOG requires a positive argument"))
         (log x)))
      ((string= u "LOG10")
       (lambda (x)
         (when (<= x 0)
           (error 'calc-error :message "LOG10 requires a positive argument"))
         (log x 10)))
      ((string= u "EXP") #'exp)
      ((string= u "SQRT")
       (lambda (x)
         (when (< x 0)
           (error 'calc-error :message "SQRT requires a non-negative argument"))
         (sqrt x)))
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
