(in-package #:calc)

(defvar *features-list*
  '("Features: + - * / ^ ! mod min max gcd lcm"
    "          sin cos tan asin acos atan"
    "          log log10 exp sqrt abs"
    "          round floor ceil"
    "          hex bin dec (base conversion)"
    "          > < = >= <= !="
    "          dup swap drop over rot nip clear depth pick tuck"
    "          and or not bitnot shl shr"
    "          M+ M- MR MC (memory register)"
    "          ? (ternary)"
    "          if/then/else (conditional)"
    "          begin/while/repeat (loop)"
    "          defun (define function)")
  "List of feature descriptions shown at startup.")

(defun print-version (&optional (stream *standard-output*))
  (format stream "calc v~A~%"
          (asdf:component-version (asdf:find-system :calc))))

(defun print-help (&optional (stream *standard-output*))
  (write-string
   "Usage: calc [options]

Calculator Monster -- RPN calculator in Common Lisp.

Options:
    --help, -h       Show this help message
    --version, -v    Show version

Interactive mode:
    Run calc with no arguments to enter the REPL.
    Type 'quit' to exit.

Expressions:
    2 3 +            Arithmetic
    0 sin            Trigonometry
    X = 42           Variables
    defun f(x) x 2 ^  User functions
    1 2 +; 3 4 +     Semicolon separates expressions

"
   stream))

(defun print-commands (&optional (stream *standard-output*))
  (write-string
   "Special commands:
    help             Show this help message
    variables        List all defined variables
    quit             Exit the calculator

"
   stream))

(defun print-variables (vars &optional (stream *standard-output*))
  (let ((var-list nil))
    (maphash (lambda (k v) (push (cons k v) var-list)) vars)
    (if var-list
        (progn
          (format stream "Defined variables:~%")
          (dolist (pair (sort var-list #'string< :key #'car))
            (format stream "  ~a = ~a~%" (car pair) (cdr pair))))
        (format stream "No variables defined.~%"))))

(defun main ()
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal))
        (interactive (interactive-stream-p *standard-input*)))
    (setf *memory* 0)
    (when interactive
      (format t "Calculator (type 'quit' or 'help' to exit)~%")
      (dolist (f *features-list*)
        (format t "~a~%" f)))
    (loop
      (when interactive
        (format t "~%")
        (force-output))
      (let ((input (read-line *standard-input* nil nil)))
        (unless input (return))
        (when (string-equal input "quit") (return))
        (when (string-equal input "help")
          (print-commands)
          (return))
        (when (string-equal input "variables")
          (print-variables vars)
          (return))
        (dolist (expr (uiop:split-string input :separator '(#\;)))
          (process-expression (string-trim '(#\Space #\Tab) expr) vars funcs))))))

(defun calc-handler (cmd)
  (declare (ignore cmd))
  (main))

(defun make-calc-command ()
  (clingon:make-command
   :name "calc"
   :description "Calculator Monster -- RPN calculator in Common Lisp."
   :version "0.0.1.0"
   :handler #'calc-handler))

(defun calc-toplevel ()
  (clingon:run (make-calc-command)))
