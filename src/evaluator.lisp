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
  (check-arity stack 2 "Stack underflow: not enough arguments")
  (cons (funcall op (second stack) (car stack)) (cddr stack)))

(defun calc-true-p (val)
  "Check if a value is truthy in calculator terms (non-nil and non-zero)."
  (and val (not (eql val 0))))

(defparameter *ternary-op-table*
  (list
    (cons "MIN3" (lambda (a b c) (min a b c)))
    (cons "MAX3" (lambda (a b c) (max a b c)))
    (cons "CLAMP" (lambda (a b c) (max b (min c a))))))

(defparameter *unary-op-table*
  (list
    (cons "SIN" (function sin))
    (cons "COS" (function cos))
    (cons "TAN" (function tan))
    (cons "ASIN" (function asin))
    (cons "ACOS" (function acos))
    (cons "ATAN" (function atan))
    (cons "SINH" (function sinh))
    (cons "COSH" (function cosh))
    (cons "TANH" (function tanh))
    (cons "ASINH" (function asinh))
    (cons "ACOSH" (function acosh))
    (cons "ATANH" (function atanh))
    (cons "LOG" (lambda (x)
         (when (<= x 0)
           (error 'calc-error :message "LOG requires a positive argument"))
         (log x)))
    (cons "LOG10" (lambda (x)
         (when (<= x 0)
           (error 'calc-error :message "LOG10 requires a positive argument"))
         (log x 10)))
    (cons "EXP" (function exp))
    (cons "SQRT" (lambda (x)
         (when (< x 0)
           (error 'calc-error :message "SQRT requires a non-negative argument"))
         (sqrt x)))
    (cons "ABS" (function abs))
    (cons "NEG" (function -))
    (cons "ROUND" (function round))
    (cons "FLOOR" (function floor))
    (cons "CEIL" (function ceiling))
    (cons "NOT" (lambda (x) (not (calc-true-p x))))
    (cons "BITNOT" (lambda (x) (lognot (truncate x))))
    (cons "HEX" (lambda (x) (format nil "~X" (truncate x))))
    (cons "BIN" (lambda (x) (format nil "~B" (truncate x))))
    (cons "DEC" (lambda (x)
                           (if (stringp x)
                               (parse-integer x :junk-allowed t)
                               x)))
    (cons "SQUARE" (lambda (x) (* x x)))
    (cons "CUBE" (lambda (x) (* x x x)))
    (cons "CUBERT" (lambda (x)
         (when (< x 0)
           (error 'calc-error :message "CUBERT requires a non-negative argument"))
         (expt x (/ 1 3))))
    (cons "RAND" (lambda (x)
         (let ((n (if (and (numberp x) (> x 0)) x 1)))
           (random (float n)))))
    (cons "RANDINT" (lambda (x)
         (let ((n (if (and (numberp x) (> x 0)) x 100)))
           (random (max 1 (truncate n))))))
    (cons "SIGNUM" (function signum))
    (cons "DEG" (lambda (x) (* x (/ 180 pi))))
    (cons "RAD" (lambda (x) (* x (/ pi 180))))
    (cons "SIND" (lambda (x) (sin (* x (/ pi 180)))))
    (cons "COSD" (lambda (x) (cos (* x (/ pi 180)))))
    (cons "TAND" (lambda (x) (tan (* x (/ pi 180)))))
))


;;; Token classifiers

(defun is-unary-func (s)
  "T when S names a unary operation (derived from *unary-op-table*)."
  (and (assoc (string-upcase s) *unary-op-table* :test (function string-equal))
       t))

(defun is-binary-op (s)
  "T when S names a binary operation (derived from *binary-op-table*)."
  (and (assoc (string-upcase s) *binary-op-table* :test (function string-equal))
       t))

(defun is-ternary-op (s)
  "T when S names a ternary operation (derived from *ternary-op-table*)."
  (and (assoc (string-upcase s) *ternary-op-table* :test (function string-equal))
       t))


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
  (member (string-upcase s) '("GET" "SET" "LEN" "PUSH" "POP" "APPEND"
                              "AMIN" "AMAX" "SORT" "REVERSE"
                              "SLICE" "INDEX")
          :test #'string=))

(defun is-string-op (s)
  (member (string-upcase s) '("STRLEN" "STRCAT" "SUBSTR" "UPPER" "LOWER" "TRIM")
          :test #'string=))
(defun is-func-op (s)
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
       (check-arity stack 2 "SWAP requires two values on the stack")
       (cons (second stack) (cons (car stack) (cddr stack))))
      ((string= u "DROP")
       (unless stack
         (error 'calc-error :message "DROP requires a value on the stack"))
       (cdr stack))
      ((string= u "OVER")
       (check-arity stack 2 "OVER requires two values on the stack")
       (cons (second stack) stack))
      ((string= u "ROT")
       (check-arity stack 3 "ROT requires three values on the stack")
       (cons (third stack)
             (cons (car stack)
                   (cons (second stack) (cdddr stack)))))
      ((string= u "NIP")
       (check-arity stack 2 "NIP requires two values on the stack")
       (cons (car stack) (cddr stack)))
      ((string= u "CLEAR")
       nil)
      ((string= u "DEPTH")
       (cons (length stack) stack))
      ((string= u "PICK")
       (check-arity stack 1 "PICK requires an index on the stack")
       (unless (numberp (car stack))
         (error 'calc-error :message "PICK index must be a number"))
       (let ((n (car stack)))
         (when (or (< n 0) (>= n (length stack))
                   (not (integerp n)))
           (error 'calc-error :message "PICK index out of range"))
         (cons (nth (- (length stack) 1 n) stack) (cdr stack))))
      ((string= u "TUCK")
       (check-arity stack 2 "TUCK requires two values on the stack")
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
       (check-arity stack 2 "GET requires array and index on the stack")
       (let ((idx (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "GET requires an array"))
         (check-index arr idx "GET index out of range")
         (cons (nth idx arr) stack)))
      ((string= u "SET")
       (check-arity stack 3 "SET requires array, index, and value on the stack")
       (let ((val (pop stack))
             (idx (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "SET requires an array"))
         (check-index arr idx "SET index out of range")
         (cons (append (subseq arr 0 idx) (list val) (subseq arr (1+ idx))) stack)))
      ((string= u "LEN")
       (check-arity stack 1 "LEN requires a value on the stack")
       (let ((val (pop stack)))
         (cond
           ((listp val) (cons (length val) stack))
           ((stringp val) (cons (length val) stack))
           (t (error 'calc-error :message "LEN requires an array or string")))))
      ((string= u "PUSH")
       (check-arity stack 2 "PUSH requires array and value on the stack")
       (let ((val (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "PUSH requires an array"))
         (cons (append arr (list val)) stack)))
      ((string= u "POP")
       (check-arity stack 1 "POP requires an array on the stack")
       (let ((arr (pop-array stack "POP requires an array")))
         (when (= (length arr) 0)
           (error 'calc-error :message "POP from empty array"))
         (cons (car (last arr)) (cons (butlast arr) stack))))
      ((string= u "APPEND")
       (check-arity stack 2 "APPEND requires two arrays on the stack")
       (let ((b (pop stack))
             (a (pop stack)))
         (unless (and (listp a) (listp b))
           (error 'calc-error :message "APPEND requires two arrays"))
         (cons (append a b) stack)))
      ((string= u "AMIN")
       (check-arity stack 1 "AMIN requires an array on the stack")
       (let ((arr (pop stack)))
         (unless (and (listp arr) arr)
           (error 'calc-error :message "AMIN requires a non-empty array"))
         (cons (reduce (cond
                        ((every #'numberp arr) #'min)
                        ((every #'stringp arr) (lambda (a b) (if (string> a b) b a)))
                        (t (error 'calc-error :message "AMIN requires all-numeric or all-string elements")))
                      arr)
           stack)))
      ((string= u "AMAX")
       (check-arity stack 1 "AMAX requires an array on the stack")
       (let ((arr (pop stack)))
         (unless (and (listp arr) arr)
           (error 'calc-error :message "AMAX requires a non-empty array"))
         (cons (reduce (cond
                        ((every #'numberp arr) #'max)
                        ((every #'stringp arr) (lambda (a b) (if (string< a b) b a)))
                        (t (error 'calc-error :message "AMAX requires all-numeric or all-string elements")))
                      arr)
           stack)))
      ((string= u "SORT")
       (check-arity stack 1 "SORT requires an array on the stack")
       (let ((arr (pop-array stack "SORT requires an array")))
         (cons (sort (copy-list arr)
                     (cond
                       ((every #'numberp arr) #'<)
                       ((every #'stringp arr) #'string<)
                       (t (error 'calc-error :message "SORT requires all-numeric or all-string elements"))))
               stack)))
      ((string= u "REVERSE")
       (check-arity stack 1 "REVERSE requires an array or string on the stack")
       (let ((arr (pop stack)))
         (unless (or (listp arr) (stringp arr))
           (error 'calc-error :message "REVERSE requires an array or string"))
         (cons (reverse arr) stack)))
      ((string= u "SLICE")
       (check-arity stack 3 "SLICE requires array, start, and length on the stack")
       (let ((len (pop stack))
             (start (pop stack))
             (arr (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "SLICE requires an array"))
         (unless (and (numberp start) (numberp len) (>= start 0) (>= len 0))
           (error 'calc-error :message "SLICE requires non-negative start and length"))
         (when (> start (length arr))
           (error 'calc-error :message "SLICE start out of range"))
         (cons (subseq arr start (min (+ start len) (length arr))) stack)))
      ((string= u "INDEX")
       (check-arity stack 2 "INDEX requires value and array on the stack")
       (let ((arr (pop stack))
             (val (pop stack)))
         (unless (listp arr)
           (error 'calc-error :message "INDEX requires an array"))
         (let ((pos (position val arr :test #'equal)))
           (cons (if pos pos -1) stack))))
      (t stack))))

;;; String operations

(defun handle-string (tok stack)
  "Handle string operations."
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "STRLEN")
       (check-arity stack 1 "STRLEN requires a string on the stack")
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "STRLEN requires a string"))
         (cons (length s) stack)))
      ((string= u "STRCAT")
       (check-arity stack 2 "STRCAT requires two strings on the stack")
       (let ((b (pop stack))
             (a (pop stack)))
         (unless (and (stringp a) (stringp b))
           (error 'calc-error :message "STRCAT requires two strings"))
         (cons (concatenate 'string a b) stack)))
      ((string= u "SUBSTR")
       (check-arity stack 3 "SUBSTR requires string, start, and length")
       (let ((len (pop stack))
             (start (pop stack))
             (s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "SUBSTR requires a string"))
         (when (or (< start 0) (> start (length s)))
           (error 'calc-error :message "SUBSTR start out of range"))
         (cons (subseq s start (min (+ start len) (length s))) stack)))
      ((string= u "UPPER")
       (check-arity stack 1 "UPPER requires a string on the stack")
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "UPPER requires a string"))
         (cons (string-upcase s) stack)))
      ((string= u "LOWER")
       (check-arity stack 1 "LOWER requires a string on the stack")
       (let ((s (pop stack)))
         (unless (stringp s)
           (error 'calc-error :message "LOWER requires a string"))
         (cons (string-downcase s) stack)))
      ((string= u "TRIM")
       (check-arity stack 1 "TRIM requires a string on the stack")
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
      (t (error 'calc-error :message (format nil "Unknown function: ~A" name))))))

(defun handle-func-op (tok stack)
  "Handle functional operations: MAP, FILTER, REDUCE."
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "MAP")
       ;; array func MAP -> array
       (check-arity stack 2 "MAP requires array and function on the stack")
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
       (check-arity stack 2 "FILTER requires array and function on the stack")
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
       (check-arity stack 3 "REDUCE requires array, function, and initial value on the stack")
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

(defun make-comparison-func (tok)
  (labels ((str-cmp (op a b)
             (case op
               (:> (and (string> a b) t))
               (:< (and (string< a b) t))
               (:= (string= a b))
               (:>= (not (string< a b)))
               (:<= (not (string> a b)))
               (:!= (not (string= a b)))))
           (list-cmp (op a b)
             (case op
               (:= (equal a b))
               (:!= (not (equal a b)))
               (:> (> (length a) (length b)))
               (:< (< (length a) (length b)))
               (:>= (>= (length a) (length b)))
               (:<= (<= (length a) (length b)))))
           (num-cmp (op a b)
             (case op
               (:> (> a b))
               (:< (< a b))
               (:= (= a b))
               (:>= (>= a b))
               (:<= (<= a b))
               (:!= (/= a b)))))
    (let ((op (cond ((string= tok ">") :>)
                    ((string= tok "<") :<)
                    ((string= tok "=") :=)
                    ((string-equal tok ">=") :>=)
                    ((string-equal tok "<=") :<=)
                    ((string-equal tok "!=") :!=))))
      (lambda (a b)
        (cond ((and (stringp a) (stringp b)) (str-cmp op a b))
              ((and (listp a) (listp b)) (list-cmp op a b))
              (t (num-cmp op a b)))))))


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
            while word
            do (unless (symbolp word)
                 (error 'calc-error :message
                        (format nil "Invalid lambda parameter: ~A" word)))
            collect (string-downcase (symbol-name word))))))

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
      (error 'calc-error :message "CALL without matching lambda"))
    (let* ((lambda-tok (nth lambda-pos tokens))
           (params (parse-lambda-params lambda-tok))
           (num-args (length params))
           (body-tokens (subseq tokens (1+ lambda-pos) (- pos num-args))))
      (check-arity stack num-args (format nil "Lambda requires ~A arguments" num-args))
      ;; Pop num-args values; accumulated push order already maps
      ;; first param to first-pushed input, no reversing needed.
      (let ((args nil))
        (loop for j from 1 to num-args do
          (push (pop stack) args))
        (let ((local-vars (make-hash-table :test 'equal)))
          (maphash (lambda (k v) (setf (gethash k local-vars) v)) vars)
          (loop for param in params
                for arg in args do
                  (setf (gethash (string-upcase param) local-vars) arg))
          (let ((result (eval-tokens body-tokens local-vars funcs)))
            (values (cons result stack) nil)))))))
(defun dispatch-token (tok stack vars funcs tokens pos)
  "Dispatch a single token. For control flow, may return (values new-stack new-pos)."
  (cond
    ((is-binary-op tok)
     (values (apply-binary-op (make-binary-func tok) stack) nil))
    ((is-ternary-op tok)
     (let ((u (string-upcase tok)))
       (check-arity stack 3 (format nil "~A requires three values on the stack" tok))
       (let ((c (pop stack))
             (b (pop stack))
             (a (pop stack)))
         (values (cons        (funcall
                        (cdr (assoc u *ternary-op-table* :test (function string-equal)))
                        a b c)

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
    ((is-stats-op tok)
     (values (handle-stats tok stack) nil))
    ((is-string-op tok)
     (values (handle-string tok stack) nil))
    ((is-func-op tok)
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
    ((member tok '("IF" "ELSE" "THEN" "WHILE" "REPEAT" "UNTIL" "BEGIN" "FOR" "NEXT")
           :test #'string-equal)
     (dispatch-control-flow tok stack vars funcs tokens pos))
    ((string= tok "?")
     ;; Ternary operator: condition true-value false-value ?
     (check-arity stack 3 "? requires three values on the stack")
     (let ((false-val (pop stack))
           (true-val (pop stack))
           (condition (pop stack)))
       (values (cons (if (calc-true-p condition)
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
           ;; Skip body tokens, the loop will process args and CALL normally
           (values stack (cons (- call-pos (length params)) nil))
           (error 'calc-error :message "Lambda without matching CALL"))))
    ((string-equal tok "CALL")
     ;; CALL - execute lambda
     (handle-lambda-call pos tokens stack vars funcs))
    (t
     (values (cons (resolve-token tok vars) stack) nil))))

(defun dispatch-control-flow (tok stack vars funcs tokens pos)
  "Handle IF/ELSE/THEN/WHILE/REPEAT/UNTIL/BEGIN/FOR/NEXT.
Returns (values stack jump); TOK is guaranteed a control token."
  (cond
    ((string-equal tok "IF")
     (unless stack
       (error 'calc-error :message "IF requires a condition on the stack"))
     (let ((condition (pop stack)))
       (if (calc-true-p condition)
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
     ;; ELSE: find the unmatched IF by walking backward, counting THENs
     (let ((if-pos nil)
           (depth 0))
       (loop for i from (1- pos) downto 0 do
         (let ((u (string-upcase (nth i tokens))))
           (cond
             ((string= u "THEN") (incf depth))
             ((string= u "IF")
              (if (= depth 0)
                  (progn
                    (setf if-pos i)
                    (return))
                  (decf depth))))))
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
       (if (calc-true-p condition)
           ;; True: continue with body
           (values stack nil)
           ;; False: find enclosing BEGIN, then skip to matching REPEAT
           (let ((begin-pos (find-matching-begin tokens pos)))
             (unless begin-pos
               (error 'calc-error :message "WHILE without matching BEGIN"))
             (let ((repeat-pos (find-matching tokens begin-pos "BEGIN" "REPEAT")))
               (values stack
                       (if repeat-pos
                           (cons (1+ repeat-pos) nil)
                           (error 'calc-error :message "WHILE without matching REPEAT"))))))))
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
       (if (calc-true-p condition)
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
     ;; FOR loop: start end FOR <var> body NEXT
     ;; The token after FOR names the loop variable (default I).
     ;; Loop state lives on a stack so nested FORs do not clash.
     (check-arity stack 2 "FOR requires start and end values on the stack")
     (let ((end-val (pop stack))
           (start-val (pop stack)))
       (unless (and (numberp start-val) (numberp end-val))
         (error 'calc-error :message "FOR bounds must be numbers"))
       (let* ((var-tok (and (< (1+ pos) (length tokens))
                            (nth (1+ pos) tokens)))
              (var-name (if (and (stringp var-tok)
                                 (> (length var-tok) 0)
                                 (alpha-char-p (char var-tok 0)))
                            (string-upcase var-tok)
                            "I")))
         (push (list var-name end-val) (gethash "%FOR-STACK" vars))
         (setf (gethash var-name vars) start-val)
         ;; Continue AT the var-name token: dispatching it pushes the
         ;; current counter value (the classic "FOR I +" idiom).
         (values stack
                 (cons (1+ pos) nil)))))  ;; continue with body
    ((string-equal tok "NEXT")
     ;; NEXT: increment the innermost loop counter; loop back or pop
     (let ((frame (first (gethash "%FOR-STACK" vars))))
       (unless frame
         (error 'calc-error :message "NEXT without matching FOR"))
       (destructuring-bind (var-name end-val) frame
         (let ((i-val (gethash var-name vars)))
           (unless i-val
             (error 'calc-error :message "NEXT without matching FOR"))
           (let ((for-pos (find-matching-next tokens pos)))
             (unless for-pos
               (error 'calc-error :message "NEXT without matching FOR"))
             (incf i-val)
             (setf (gethash var-name vars) i-val)
             (if (<= i-val end-val)
                 (values stack (cons (1+ for-pos) nil))  ;; loop back
                 (progn
                   (pop (gethash "%FOR-STACK" vars))
                   (values stack nil))))))))  ;; exit loop
    (t (error 'calc-error :message "Not a control-flow token: ~A" tok))))

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
  "Find the FOR matching the NEXT at position POS.
Scans backward so the NEAREST unmatched FOR wins (correct for nesting)."
  (let ((depth 0))
    (loop for i from (1- pos) downto 0 do
      (let ((u (string-upcase (nth i tokens))))
        (cond
          ((string= u "NEXT") (incf depth))
          ((string= u "FOR")
           (if (= depth 0)
               (return-from find-matching-next i)
               (decf depth))))))))

;;; Main evaluator

(defun expand-macro-call (tok stack vars funcs)
  "Expand and evaluate a macro call. Returns (values new-stack expanded-p)."
  (let ((macro (gethash (string-upcase tok) *macros*)))
    (if macro
        (let ((param-names (getf macro :args))
              (body (getf macro :body))
              (num-args (length (getf macro :args))))
          ;; Pop arguments from stack
          (check-arity stack num-args (format nil "~A requires ~A arguments" tok num-args))
          ;; Pop num-args values; accumulated push order already maps
          ;; first param to first-pushed input, no reversing needed.
          (let ((args nil))
            (loop for j from 1 to num-args do
              (push (pop stack) args))
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

(defun eval-tokens (tokens vars funcs)
  (let ((stack nil)
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

(defun eval-rpn (expr vars funcs)
  "Tokenize EXPR and evaluate; see EVAL-TOKENS for the evaluator proper."
  (eval-tokens (tokenize expr) vars funcs))
(defun is-stats-op (s)
  (member (string-upcase s) '("MEAN" "MEDIAN" "STDDEV" "SUM" "COUNT"
                              "VARIANCE" "RANGE" "MODE")
          :test #'string=))
(defun handle-stats (tok stack)
  (let ((u (string-upcase tok)))
    (cond
      ((string= u "MEAN")
       (check-arity stack 1 "MEAN requires an array on the stack")
       (let ((arr (pop-array stack "MEAN requires an array")))
         (when (null arr)
           (error 'calc-error :message "MEAN requires a non-empty array"))
         (cons (mean-of arr) stack)))
      ((string= u "MEDIAN")
       (check-arity stack 1 "MEDIAN requires an array on the stack")
       (let ((arr (pop-array stack "MEDIAN requires an array")))
         (when (null arr)
           (error 'calc-error :message "MEDIAN requires a non-empty array"))
         (let* ((sorted (sort (copy-list arr) #'<))
                (n (length sorted))
                (mid (floor n 2)))
           (cons (if (oddp n)
                     (nth mid sorted)
                     (/ (+ (nth (1- mid) sorted) (nth mid sorted)) 2))
                 stack))))
      ((string= u "STDDEV")
       (check-arity stack 1 "STDDEV requires an array on the stack")
       (let ((arr (pop-array stack "STDDEV requires an array")))
         (when (< (length arr) 2)
           (error 'calc-error :message "STDDEV requires at least two elements"))
         (let* ((mean (mean-of arr))
                (variance (/ (apply #'+ (mapcar (lambda (x) (expt (- x mean) 2)) arr))
                             (length arr))))
           (cons (sqrt variance) stack))))
      ((string= u "VARIANCE")
       (check-arity stack 1 "VARIANCE requires an array on the stack")
       (let ((arr (pop-array stack "VARIANCE requires an array")))
         (when (< (length arr) 2)
           (error 'calc-error :message "VARIANCE requires at least two elements"))
         (let* ((mean (mean-of arr))
                (variance (/ (apply #'+ (mapcar (lambda (x) (expt (- x mean) 2)) arr))
                             (length arr))))
           (cons variance stack))))
      ((string= u "RANGE")
       (check-arity stack 1 "RANGE requires an array on the stack")
       (let ((arr (pop stack)))
         (unless (and (listp arr) arr)
           (error 'calc-error :message "RANGE requires a non-empty array"))
         (cons (- (reduce #'max arr) (reduce #'min arr)) stack)))
      ((string= u "MODE")
       (check-arity stack 1 "MODE requires an array on the stack")
       (let ((arr (pop stack)))
         (unless (and (listp arr) arr)
           (error 'calc-error :message "MODE requires a non-empty array"))
         (let ((sorted (sort (copy-list arr) #'<)))
           (loop with best = (car sorted)
                 with best-n = 0
                 for val in sorted
                 for n = (count val sorted :test #'equal)
                 when (> n best-n)
                   do (setf best val best-n n)
                 finally (return (cons best stack))))))
      ((string= u "SUM")
       (check-arity stack 1 "SUM requires an array on the stack")
       (let ((arr (pop-array stack "SUM requires an array")))
         (cons (sum-array arr) stack)))
      ((string= u "COUNT")
       (check-arity stack 1 "COUNT requires an array on the stack")
       (let ((arr (pop-array stack "COUNT requires an array")))
         (cons (count-array arr) stack)))
      (t stack))))
(defun ncr (n r)
  (unless (and (integerp n) (integerp r) (>= n 0) (>= r 0) (<= r n))
    (error 'calc-error :message "NCR requires 0 <= r <= n"))
  (/ (factorial-func n) (* (factorial-func r) (factorial-func (- n r)))))


(defun sum-array (arr)
  "Sum of a list of numbers."
  (apply #'+ arr))

(defun count-array (arr)
  "Number of elements in a list."
  (length arr))

(defun factorial-func (n)
  "Delegate to the validated factorial implementation."
  (factorial n))
(defun npr (n r)
  (unless (and (integerp n) (integerp r) (>= n 0) (>= r 0) (<= r n))
    (error 'calc-error :message "NPR requires 0 <= r <= n"))
  (/ (factorial-func n) (factorial-func (- n r))))
(defparameter *binary-op-table*
  (list
    (cons "+" (lambda (a b)
       (cond
         ((and (stringp a) (stringp b)) (concatenate 'string a b))
         ((and (listp a) (listp b))
          (when (/= (length a) (length b))
            (error 'calc-error :message "+ requires equal-length arrays"))
          (mapcar #'+ a b))
         ((and (listp a) (numberp b)) (mapcar (lambda (x) (+ x b)) a))
         ((and (numberp a) (listp b)) (mapcar (lambda (x) (+ a x)) b))
         (t (+ a b)))))
    (cons "-" (lambda (a b)
       (cond
         ((and (listp a) (listp b))
          (when (/= (length a) (length b))
            (error 'calc-error :message "- requires equal-length arrays"))
          (mapcar #'- a b))
         ((and (listp a) (numberp b)) (mapcar (lambda (x) (- x b)) a))
         ((and (numberp a) (listp b)) (mapcar (lambda (x) (- a x)) b))
         (t (- a b)))))
    (cons "*" (lambda (a b)
       (cond
         ((and (stringp a) (numberp b))
          (apply #'concatenate 'string
                 (make-list (max 0 (truncate b)) :initial-element a)))
         ((and (numberp a) (stringp b))
          (apply #'concatenate 'string
                 (make-list (max 0 (truncate a)) :initial-element b)))
         ((and (listp a) (listp b))
          (when (/= (length a) (length b))
            (error 'calc-error :message "* requires equal-length arrays"))
          (mapcar #'* a b))
         ((and (listp a) (numberp b)) (mapcar (lambda (x) (* x b)) a))
         ((and (numberp a) (listp b)) (mapcar (lambda (x) (* a x)) b))
         (t (* a b)))))
    (cons "/" (lambda (a b)
       (when (or (and (numberp b) (= b 0))
                 (and (listp b) (member 0 b :test #'=)))
         (error 'calc-error :message "Division by zero"))
       (cond
         ((and (listp a) (listp b))
          (when (/= (length a) (length b))
            (error 'calc-error :message "/ requires equal-length arrays"))
          (mapcar #'/ a b))
         ((and (listp a) (numberp b)) (mapcar (lambda (x) (/ x b)) a))
         ((and (numberp a) (listp b)) (mapcar (lambda (x) (/ a x)) b))
         (t (/ a b)))))
    (cons "^" #'expt)
    (cons "MOD" (lambda (a b)
       (when (= b 0)
         (error 'calc-error :message "Division by zero (MOD)"))
       (mod a b)))
    (cons "MIN" #'min)
    (cons "MAX" #'max)
    (cons "GCD" #'gcd)
    (cons "LCM" #'lcm)
    (cons "LOGAND" #'logand)
    (cons "LOGIOR" #'logior)
    (cons "LOGXOR" #'logxor)
    (cons "LOGEQV" (lambda (a b) (lognot (logxor a b))))
    (cons "AND" (lambda (a b) (and a b)))
    (cons "OR" (lambda (a b) (or a b)))
    (cons "XOR" (lambda (a b) (not (eql a b))))
    (cons "NAND" (lambda (a b) (not (and a b))))
    (cons "NOR" (lambda (a b) (not (or a b))))
    (cons "SHL" (lambda (a b) (ash (truncate a) b)))
    (cons "SHR" (lambda (a b) (ash (truncate a) (- b))))
    (cons "HYPOT" (lambda (a b) (sqrt (+ (* a a) (* b b)))))
    (cons "ATAN2" #'atan)
    (cons "POW" #'expt)
    (cons "IDIV" (lambda (a b)
       (when (= b 0)
         (error 'calc-error :message "Division by zero (IDIV)"))
       (truncate a b)))
    (cons "NPR" #'npr)
    (cons "NCR" #'ncr)
    (cons "IN" (lambda (a b)
       (cond
         ((and (stringp a) (stringp b))
          (if (search a b) t nil))
         (t (if (member a b :test #'equal) t nil)))))
    (cons "NROOT" (lambda (a b)
       (when (= b 0)
         (error 'calc-error :message "NROOT requires a non-zero degree"))
       (when (and (< a 0) (evenp b))
         (error 'calc-error :message "NROOT of a negative with an even degree"))
       (let ((r (expt (abs a) (/ 1 b))))
         (if (< a 0) (- r) r))))
)
  "Token -> binary operation. Lookup is string-equal (case-insensitive)."
)
(defun mean-of (arr)
  "Arithmetic mean of a non-empty numeric list."
  (/ (apply #'+ arr) (length arr)))
(defun check-arity (stack n message)
  "Signal calc-error unless STACK holds at least N values."
  (when (< (length stack) n)
    (error 'calc-error :message message)))

(defun make-binary-func (tok)
  "Return the binary operation function for token TOK, or NIL."
  (cdr (assoc tok *binary-op-table* :test #'string-equal)))

(defun make-unary-func (tok)
  "Return the unary operation function for token TOK, or NIL."
  (cdr (assoc (string-upcase tok) *unary-op-table* :test #'string-equal)))
(defun pop-array (stack message)
  "Pop STACK expecting an array; signal calc-error otherwise."
  (let ((arr (pop stack)))
    (unless (listp arr)
      (error 'calc-error :message message))
    arr))

(defun check-index (arr idx message)
  "Signal calc-error unless IDX is a valid index into ARR."
  (when (or (< idx 0) (>= idx (length arr)))
    (error 'calc-error :message message)))
