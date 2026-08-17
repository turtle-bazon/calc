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
                              "HEX" "BIN" "DEC"
                              "SQUARE" "CUBE" "CUBERT")
          :test #'string=))

(defun is-binary-op (s)
  (member (string-upcase s) '("+" "-" "*" "/" "^" "MOD" "MIN" "MAX"
                              "GCD" "LCM" "LOGAND" "LOGIOR" "LOGXOR" "LOGEQV"
                              "AND" "OR" "XOR" "NAND" "NOR"
                              "SHL" "SHR" "HYPOT" "ATAN2" "POW")
          :test #'string=))

(defun is-ternary-op (s)
  (member (string-upcase s) '("MIN3" "MAX3" "CLAMP")
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


(defun is-array-op (s)
  (member (string-upcase s) '("GET" "SET" "LEN" "PUSH" "POP" "APPEND")
          :test #'string=))

(defun is-string-op (s)
  (member (string-upcase s) '("STRLEN" "STRCAT" "SUBSTR" "UPPER" "LOWER" "TRIM")
          :test #'string=))(defun is-func-op (s)
  (member (string-upcase s) '("MAP" "FILTER" "REDUCE")
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

;;; Array operations

(defun handle-array (tok stack)
  "Handle array operations."
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "GET")
       (when (< (length stack) 2)
         (error 'calc-error :message "GET requires array and index on the stack"))
       (let ((idx (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "GET requires an array"))
         (when (or (< idx 0) (>= idx (length arr)))
           (error 'calc-error :message "GET index out of range"))
         (cons (nth idx arr) stack)))
      ((string= u "SET")
       (when (< (length stack) 3)
         (error 'calc-error :message "SET requires array, index, and value on the stack"))
       (let ((val (pop stack))
             (idx (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "SET requires an array"))
         (when (or (< idx 0) (>= idx (length arr)))
           (error 'calc-error :message "SET index out of range"))
         (cons (append (subseq arr 0 idx) (list val) (subseq arr (1+ idx))) stack)))
      ((string= u "LEN")
       (when (< (length stack) 1)
         (error 'calc-error :message "LEN requires a value on the stack"))
       (let ((val (pop stack)))
         (cond
           ((listp val) (cons (length val) stack))
           ((stringp val) (cons (length val) stack))
           (t (error 'calc-error :message "LEN requires an array or string")))))
      ((string= u "PUSH")
       (when (< (length stack) 2)
         (error 'calc-error :message "PUSH requires array and value on the stack"))
       (let ((val (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "PUSH requires an array"))
         (cons (append arr (list val)) stack)))
      ((string= u "POP")
       (when (< (length stack) 1)
         (error 'calc-error :message "POP requires an array on the stack"))
       (let ((arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "POP requires an array"))
         (when (= (length arr) 0)
           (error 'calc-error :message "POP from empty array"))
         (cons (car (last arr)) (cons (butlast arr) stack))))
      ((string= u "APPEND")
       (when (< (length stack) 2)
         (error 'calc-error :message "APPEND requires two arrays on the stack"))
       (let ((b (pop stack))
             (a (pop stack)))
         (unless (and (listp a) (listp b))
           (error 'calc-error :message "APPEND requires two arrays"))
         (cons (append a b) stack)))
      (t stack))))

;;; String operations

(defun handle-string (tok stack)
  "Handle string operations."
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "STRLEN")
       (when (< (length stack) 1)
         (error 'calc-error :message "STRLEN requires a string on the stack"))
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "STRLEN requires a string"))
         (cons (length s) stack)))
      ((string= u "STRCAT")
       (when (< (length stack) 2)
         (error 'calc-error :message "STRCAT requires two strings on the stack"))
       (let ((b (pop stack))
             (a (pop stack)))
         (unless (and (stringp a) (stringp b))
           (error 'calc-error :message "STRCAT requires two strings"))
         (cons (concatenate 'string a b) stack)))
      ((string= u "SUBSTR")
       (when (< (length stack) 3)
         (error 'calc-error :message "SUBSTR requires string, start, and length"))
       (let ((len (pop stack))
             (start (pop stack))
             (s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "SUBSTR requires a string"))
         (when (or (< start 0) (> start (length s)))
           (error 'calc-error :message "SUBSTR start out of range"))
         (cons (subseq s start (min (+ start len) (length s))) stack)))
      ((string= u "UPPER")
       (when (< (length stack) 1)
         (error 'calc-error :message "UPPER requires a string on the stack"))
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "UPPER requires a string"))
         (cons (string-upcase s) stack)))
      ((string= u "LOWER")
       (when (< (length stack) 1)
         (error 'calc-error :message "LOWER requires a string on the stack"))
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "LOWER requires a string"))
         (cons (string-downcase s) stack)))
      ((string= u "TRIM")
       (when (< (length stack) 1)
         (error 'calc-error :message "TRIM requires a string on the stack"))
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "TRIM requires a string"))
         (cons (string-trim '(#\Space #\Tab #\Newline) s) stack)))
      (t stack))))

;;; Function constructors

(defun resolve-func (name)
  "Resolve a function name to a callable function."
  (let ((u (string-upcase name)))
    (cond
      ((is-unary-func u) (make-unary-func u))
      ((is-binary-op u) (make-binary-func u))
      ((is-comparison u) (make-comparison-func u))
      ((string= u "NOT") (lambda (x) (not x)))
      ((string= u "ABS") #'abs)
      ((string= u "NEG") #'-)
      ((string= u "ROUND") #'round)
      ((string= u "FLOOR") #'floor)
      ((string= u "CEIL") #'ceiling)
      (t (error 'calc-error :message (format nil "Unknown function: ~A" name))))))

(defun handle-func-op (tok stack)
  "Handle functional operations: MAP, FILTER, REDUCE."
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "MAP")
       ;; array func MAP -> array
       (when (< (length stack) 2)
         (error 'calc-error :message "MAP requires array and function on the stack"))
       (let ((func-name (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "MAP requires an array"))
         (unless (stringp func-name)
           (error 'calc-error :message "MAP requires a function name as string"))
         (let ((func (resolve-func func-name)))
           (cons (mapcar func arr) stack))))
      ((string= u "FILTER")
       ;; array func FILTER -> array
       (when (< (length stack) 2)
         (error 'calc-error :message "FILTER requires array and function on the stack"))
       (let ((func-name (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "FILTER requires an array"))
         (unless (stringp func-name)
           (error 'calc-error :message "FILTER requires a function name as string"))
         (let ((pred (resolve-func func-name)))
           (cons (remove-if-not pred arr) stack))))
      ((string= u "REDUCE")
       ;; array func init REDUCE -> value
       (when (< (length stack) 3)
         (error 'calc-error :message "REDUCE requires array, function, and initial value on the stack"))
       (let ((init (pop stack))
             (func-name (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "REDUCE requires an array"))
         (unless (stringp func-name)
           (error 'calc-error :message "REDUCE requires a function name as string"))
         (let ((func (resolve-func func-name)))
           (cons (reduce func arr :initial-value init) stack))))
      (t stack))))
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
    ((string-equal tok "LOGEQV") (lambda (a b) (lognot (logxor a b))))
    ((string-equal tok "AND") (lambda (a b) (and a b)))
    ((string-equal tok "OR") (lambda (a b) (or a b)))
    ((string-equal tok "XOR") (lambda (a b) (not (eql a b))))
    ((string-equal tok "NAND") (lambda (a b) (not (and a b))))
    ((string-equal tok "NOR") (lambda (a b) (not (or a b))))
    ((string-equal tok "SHL") (lambda (a b) (ash (truncate a) b)))
    ((string-equal tok "SHR") (lambda (a b) (ash (truncate a) (- b))))
    ((string-equal tok "HYPOT") (lambda (a b) (sqrt (+ (* a a) (* b b)))))
    ((string-equal tok "ATAN2") #'atan)
    ((string-equal tok "POW") #'expt)))

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
                               x)))
      ((string= u "SQUARE") (lambda (x) (* x x)))
      ((string= u "CUBE") (lambda (x) (* x x x)))
      ((string= u "CUBERT")
       (lambda (x)
         (when (< x 0)
           (error 'calc-error :message "CUBERT requires a non-negative argument"))
         (expt x (/ 1 3)))))))

;;; Factorial

(defun factorial (n)
  (unless (and (integerp n) (>= n 0))
    (error 'calc-error :message (format nil "Factorial requires a non-negative integer, got ~A" n)))
  (apply #'* (loop for i from 1 to n collect i)))

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

(defun lambda-token-p (tok)
  "Check if a token is a lambda definition like ( x )."
  (and (> (length tok) 1)
       (char= (char tok 0) #\()
       (char= (char tok (1- (length tok))) (char ")" 0))))

(defun parse-lambda-params (tok)
  "Parse parameter names from a lambda token like ( x y )."
  (let ((content (subseq tok 1 (1- (length tok)))))
    (with-input-from-string (s content)
      (loop for word = (read s nil nil)
            while word collect (string-downcase (symbol-name word))))))

(defun find-call-forward (tokens start)
  "Find the next CALL token starting from position START."
  (loop for i from start below (length tokens)
        when (string-equal (nth i tokens) "CALL")
          return i))

(defun handle-lambda-call (pos tokens stack vars funcs)
  "Handle CALL token by finding the enclosing lambda and executing it."
  (let ((lambda-pos nil))
    (loop for i from (1- pos) downto 0 do
      (when (lambda-token-p (nth i tokens))
        (setf lambda-pos i)
        (return)))
    (unless lambda-pos
      (error (quote calc-error) :message "CALL without matching lambda"))
    (let* ((lambda-tok (nth lambda-pos tokens))
           (params (parse-lambda-params lambda-tok))
           (num-args (length params))
           (body-tokens (subseq tokens (1+ lambda-pos) (- pos num-args))))
      (when (< (length stack) num-args)
        (error (quote calc-error) :message (format nil "Lambda requires ~A arguments" num-args)))
      (let ((args nil))
        (loop for j from 1 to num-args do
          (push (pop stack) args))
        (setf args (nreverse args))
        (let ((local-vars (make-hash-table :test (quote equal))))
          (maphash (lambda (k v) (setf (gethash k local-vars) v)) vars)
          (loop for param in params
                for arg in args do
                  (setf (gethash (string-upcase param) local-vars) arg))
          (let ((result (eval-rpn (format nil "~{~A~^ ~}" body-tokens) local-vars funcs)))
            (values (cons result stack) nil)))))))
(defun dispatch-token (tok stack vars funcs tokens pos)
  "Dispatch a single token. For control flow, may return (values new-stack new-pos)."
  (cond
    ((is-binary-op tok)
     (values (apply-binary-op (make-binary-func tok) stack) nil))
    ((is-ternary-op tok)
     (let ((u (string-upcase tok)))
       (when (< (length stack) 3)
         (error 'calc-error :message (format nil "~A requires three values on the stack" tok)))
       (let ((c (pop stack))
             (b (pop stack))
             (a (pop stack)))
         (values (cons (cond
                         ((string= u "MIN3") (min a b c))
                         ((string= u "MAX3") (max a b c))
                         ((string= u "CLAMP") (max b (min c a)))
                         (t (error 'calc-error :message (format nil "Unknown ternary op: ~A" tok))))
                       stack)
                 nil))))
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
    ((is-array-op tok)
     (values (handle-array tok stack) nil))
    ((is-string-op tok)
     (values (handle-string tok stack) nil))((is-func-op tok)
     (values (handle-func-op tok stack) nil))

    ;; String literals
    ((and (> (length tok) 1) (char= (char tok 0) #\") (char= (char tok (1- (length tok))) #\"))
     (values (cons (subseq tok 1 (1- (length tok))) stack) nil))
    ;; Array literals [ ... ]
    ((and (> (length tok) 1) (char= (char tok 0) #\[) (char= (char tok (1- (length tok))) #\]))
     (let ((inner (subseq tok 1 (1- (length tok)))))
       (if (> (length inner) 0)
           ;; Tokenize the inner content and collect all values into a list
           (let ((arr-tokens (tokenize inner))
                 (arr-stack nil)
                 (arr-pos 0))
             (loop while (< arr-pos (length arr-tokens)) do
               (let ((at (nth arr-pos arr-tokens)))
                 (multiple-value-bind (new-stack jump)
                     (dispatch-token at arr-stack vars funcs arr-tokens arr-pos)
                   (setf arr-stack new-stack)
                   (if jump
                       (setf arr-pos (car jump))
                       (incf arr-pos)))))
             ;; arr-stack is in reverse order (last element first), so reverse it
             (values (cons (reverse arr-stack) stack) nil))
           (values (cons nil stack) nil))))
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
       (values (cons (if (and (numberp condition) (not (= condition 0)))
                         true-val
                         false-val)
                     stack)
               nil)))
    ((or (string= tok "(") (string= tok ")"))
     (values stack nil))
    ((lambda-token-p tok)
       ;; Lambda token - skip body tokens
       (let ((params (parse-lambda-params tok))
             (call-pos (find-call-forward tokens (1+ pos))))
         (if call-pos
             (let ((body-len (- call-pos pos 1 (length params))))
               ;; Skip body tokens, the loop will process args and CALL normally
               (values stack (cons (- call-pos (length params)) nil)))
             (error (quote calc-error) :message "Lambda without matching CALL"))))
((string-equal tok "CALL")
       ;; CALL - execute lambda
       (handle-lambda-call pos tokens stack vars funcs))
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

(defun expand-macro-call (tok stack vars funcs)
  "Expand and evaluate a macro call. Returns (values new-stack expanded-p)."
  (let ((macro (gethash (string-upcase tok) *macros*)))
    (if macro
        (let ((param-names (getf macro :args))
              (body (getf macro :body))
              (num-args (length (getf macro :args))))
          ;; Pop arguments from stack
          (when (< (length stack) num-args)
            (error 'calc-error :message (format nil "~A requires ~A arguments" tok num-args)))
          (let ((args nil))
            (loop for j from 1 to num-args do
              (push (pop stack) args))
            (setf args (nreverse args))
            ;; Bind arguments as variables and evaluate body
            (let ((local-vars (make-hash-table :test #'equal)))
              ;; Copy existing vars
              (maphash (lambda (k v) (setf (gethash k local-vars) v)) vars)
              ;; Bind macro parameters
              (loop for param in param-names
                    for arg in args do
                      (setf (gethash (string-upcase param) local-vars) arg))
              (let ((result (eval-rpn body local-vars funcs)))
                (values (cons result stack) t)))))
        (values stack nil))))

(defun eval-rpn (expr vars funcs)
  (let ((tokens (tokenize expr))
        (stack nil)
        (pos 0))
    (loop while (< pos (length tokens)) do
      (let ((tok (nth pos tokens)))
        ;; Check if this is a macro call
        (multiple-value-bind (new-stack expanded-p)
            (expand-macro-call tok stack vars funcs)
          (if expanded-p
              (progn
                (setf stack new-stack)
                (incf pos))
              ;; Not a macro, use normal dispatch
              (multiple-value-bind (new-stack jump)
                  (dispatch-token tok stack vars funcs tokens pos)
                (setf stack new-stack)
                (if jump
                    (setf pos (car jump))
                    (incf pos)))))))
    (if stack (car stack) nil)))
