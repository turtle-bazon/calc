(in-package :calc)

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
                              "WHILE" "REPEAT" "UNTIL" "BEGIN")
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

;;; Control flow: find matching end token

(defun find-matching (tokens start skip-word end-word)
  "Find the matching end-word for skip-word starting at position START.
   Handles nested skip-words. Returns index of end-word or NIL."
  (let ((depth 0)
        (len (length tokens))
        (found-open nil))
    (dotimes (i len)
      (when (>= i start)
        (let ((u (string-upcase (nth i tokens))))
          (cond
            ((string= u skip-word)
             (incf depth)
             (setf found-open t))
            ((and found-open (string= u end-word))
             (decf depth)
             (when (= depth 0)
               (return-from find-matching i)))))))
    nil))

;;; Token dispatch

(defun dispatch-token (tok stack vars funcs tokens pos)
  "Dispatch a single token. For control flow, may return (values new-stack new-pos)."
  (cond
    ((is-binary-op tok)
     (values (apply-binary-op (make-binary-func tok) stack) nil))
    ((is-comparison tok)
     (values (apply-binary-op (make-comparison-func tok) stack) nil))
    ((is-unary-func tok)
     (values (apply-unary-op (make-unary-func tok) stack) nil))
    ((string= tok "!")
     (values (apply-unary-op #'factorial stack) nil))
    ((is-memory-op tok)
     (values (handle-memory tok stack) nil))
    ((is-stack-op tok)
     (values (handle-stack tok stack) nil))
    ((string-equal tok "IF")
     (unless stack
       (error 'calc-error :message "IF requires a condition on the stack"))
     (let ((condition (pop stack)))
       (if condition
           ;; True: find ELSE or THEN, execute true branch
           (let ((else-pos (find-matching tokens pos "IF" "ELSE"))
                 (then-pos (find-matching tokens pos "IF" "THEN")))
             (unless then-pos
               (error 'calc-error :message "IF without matching THEN"))
             (values stack
                     (if (and else-pos (< else-pos then-pos))
                         (cons (1+ pos) (1- else-pos))  ;; execute up to ELSE
                         (cons (1+ pos) (1- then-pos)))))  ;; execute up to THEN
           ;; False: find ELSE or THEN, skip to it
           (let ((else-pos (find-matching tokens pos "IF" "ELSE"))
                 (then-pos (find-matching tokens pos "IF" "THEN")))
             (unless then-pos
               (error 'calc-error :message "IF without matching THEN"))
             (values stack
                     (if else-pos
                         (cons (1+ else-pos) (1- then-pos))  ;; skip to ELSE, execute false branch
                         (cons (1+ then-pos) nil)))))))  ;; no ELSE, skip past THEN
    ((string-equal tok "ELSE")
     ;; ELSE: find the enclosing IF, then find matching THEN
     (let ((if-pos nil))
       (dotimes (i pos)
         (when (and (string-equal (nth i tokens) "IF")
                    (find-matching tokens i "IF" "THEN"))
           (setf if-pos i)))
       (unless if-pos
         (error 'calc-error :message "ELSE without matching IF"))
       (let ((then-pos (find-matching tokens if-pos "IF" "THEN")))
         (unless then-pos
           (error 'calc-error :message "ELSE without matching THEN"))
         (values stack (cons then-pos nil)))))
    ((string-equal tok "THEN")
     ;; THEN marks end of if block, continue after it
     (values stack nil))
    ((string-equal tok "WHILE")
     (unless stack
       (error 'calc-error :message "WHILE requires a condition on the stack"))
     (let ((condition (pop stack)))
       (if condition
           ;; True: continue with body
           (values stack nil)
           ;; False: skip to matching REPEAT
           (let ((repeat-pos (find-matching tokens (1+ pos) "BEGIN" "REPEAT")))
             (values stack
                     (if repeat-pos
                         (cons (1+ repeat-pos) nil)
                         (error 'calc-error :message "WHILE without matching REPEAT")))))))
    ((string-equal tok "REPEAT")
     ;; REPEAT loops back to matching BEGIN
     (let ((begin-pos (find-matching-begin tokens pos)))
       (values stack
              (if begin-pos
                  (cons begin-pos nil)
                  (error 'calc-error :message "REPEAT without matching BEGIN")))))
    ((string-equal tok "UNTIL")
     (unless stack
       (error 'calc-error :message "UNTIL requires a condition on the stack"))
     (let ((condition (pop stack)))
       (if condition
           ;; True: exit loop
           (values stack nil)
           ;; False: loop back
           (let ((begin-pos (find-matching-begin tokens pos)))
             (values stack
                    (if begin-pos
                        (cons begin-pos nil)
                        (error 'calc-error :message "UNTIL without matching BEGIN")))))))
    ((string-equal tok "BEGIN")
     ;; BEGIN marks start of loop, just continue
     (values stack nil))
    ((string-equal tok "FOR")
     ;; FOR loop: start end FOR ... NEXT
     (when (< (length stack) 2)
       (error 'calc-error :message "FOR requires start and end values on the stack"))
     (let ((end-val (pop stack))
           (start-val (pop stack)))
       (unless (and (numberp start-val) (numberp end-val))
         (error 'calc-error :message "FOR bounds must be numbers"))
       (setf (gethash "I" vars) start-val
             (gethash "%FOR-END" vars) end-val)
       (values stack
               (cons (1+ pos) nil))))  ;; continue with body
     ((string-equal tok "NEXT")
     ;; NEXT: increment counter and loop back to FOR
     (let ((i-val (gethash "I" vars))
           (end-val (gethash "%FOR-END" vars)))
       (unless i-val
         (error 'calc-error :message "NEXT without matching FOR"))
       (let ((for-pos (find-matching-next tokens pos)))
         (unless for-pos
           (error 'calc-error :message "NEXT without matching FOR"))
           (incf i-val)
           (setf (gethash "I" vars) i-val)
           (if (and end-val (<= i-val end-val))
               (values stack (cons (1+ for-pos) nil))  ;; loop back
               (progn
                 (remhash "%FOR-END" vars)
                 (values stack nil))))))  ;; exit loop
    ((string= tok "?")
     ;; Ternary operator: condition true-value false-value ?
     (when (< (length stack) 3)
       (error 'calc-error :message "? requires three values on the stack"))
     (let ((false-val (pop stack))
           (true-val (pop stack))
           (condition (pop stack)))
       (values (cons (if condition true-val false-val) stack) nil)))
    ((or (string= tok "(") (string= tok ")"))
     (values stack nil))
    (t
     (values (cons (resolve-token tok vars) stack) nil))))

(defun find-matching-begin (tokens pos)
  "Find matching BEGIN for a REPEAT or UNTIL at position POS."
  (let ((depth 0)
        (len (length tokens)))
    (dotimes (i len)
      (when (<= i (1- pos))
        (let ((u (string-upcase (nth i tokens))))
          (cond
            ((or (string= u "REPEAT") (string= u "UNTIL") (string= u "WHILE"))
             (incf depth))
            ((and (> depth 0) (string= u "BEGIN"))
             (decf depth))
            ((and (= depth 0) (string= u "BEGIN"))
             (return-from find-matching-begin i))))))
    nil))

(defun find-matching-next (tokens pos)
  "Find matching FOR for a NEXT at position POS."
  (let ((depth 0)
        (len (length tokens)))
    (dotimes (i len)
      (when (<= i (1- pos))
        (let ((u (string-upcase (nth i tokens))))
          (cond
            ((string= u "NEXT")
             (incf depth))
            ((and (> depth 0) (string= u "FOR"))
             (decf depth))
            ((and (= depth 0) (string= u "FOR"))
             (return-from find-matching-next i))))))
    nil))

;;; Main evaluator

(defun eval-rpn (expr vars funcs)
  (let ((tokens (tokenize expr))
        (stack nil)
        (pos 0))
    (loop while (< pos (length tokens)) do
      (let ((tok (nth pos tokens)))
        (multiple-value-bind (new-stack jump)
            (dispatch-token tok stack vars funcs tokens pos)
          (setf stack new-stack)
          (if jump
              (setf pos (car jump))
              (incf pos)))))
    (if stack (car stack) nil)))
