(in-package #:calc)

(defvar *history* nil "List of previous input commands.")
(defvar *history-file* (merge-pathnames ".calc_history" (user-homedir-pathname))
  "File to store command history.")

(defun load-history ()
  "Load command history from file."
  (when (probe-file *history-file*)
    (with-open-file (stream *history-file* :direction :input :if-does-not-exist nil)
      (when stream
        (setf *history*
              (loop for line = (read-line stream nil nil)
                    while line
                    collect line))))))

(defun save-history ()
  "Save command history to file."
  (with-open-file (stream *history-file* :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (when stream
      (dolist (line *history*)
        (format stream "~a~%" line)))))

(defun add-to-history (input)
  "Add input to history if not empty and not duplicate of last."
  (when (and (> (length input) 0)
             (or (null *history*)
                 (not (string= input (first *history*)))))
    (push input *history*)
    ;; Keep only last 1000 commands
    (when (> (length *history*) 1000)
      (setf *history* (subseq *history* 0 1000)))))

(defvar *features-list*
  '("Features: + - * / ^ ! mod min max gcd lcm"
    "          sin cos tan asin acos atan"
    "          log log10 exp sqrt abs square cube cubert"
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

(defun run-file (filename vars funcs)
  "Execute a .calc script file."
  (let ((interactive (interactive-stream-p *standard-input*)))
    (with-open-file (stream filename :direction :input :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil nil)
              while line do
                (when interactive
                  (format t "~a~%" line)
                  (force-output))
                (dolist (expr (uiop:split-string line :separator '(#\;)))
                  (let ((trimmed (string-trim '(#\Space #\Tab) expr)))
                    (when (> (length trimmed) 0)
                      (process-expression trimmed vars funcs)))))))))

(defun main (&optional args)
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal))
        (interactive (interactive-stream-p *standard-input*)))
    (setf *memory* 0)
    ;; Load history
    (load-history)
    ;; Check for file argument
    (when (and args (> (length args) 0))
      (let ((filename (first args)))
        (run-file filename vars funcs)
        (save-history)
        (return-from main)))
    (when interactive
      (format t "Calculator (type 'quit' or 'help' to exit)~%")
      (dolist (f *features-list*)
        (format t "~a~%" f)))
    (unwind-protect
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
            (add-to-history input)
            (dolist (expr (uiop:split-string input :separator '(#\;)))
              (process-expression (string-trim '(#\Space #\Tab) expr) vars funcs))))
      ;; Save history on exit
      (save-history))))

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
