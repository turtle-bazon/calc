(in-package #:calc)

(defvar *features-list*
  '("Features: + - * / ^ !"
    "          sin cos tan asin acos atan"
    "          log log10 exp sqrt abs"
    "          hex bin dec (base conversion)"
    "          > < = >= <= !="
    "          dup swap drop over rot nip"
    "          and or not bitnot shl shr"
    "          min max gcd lcm round floor ceil"
    "          defun (define function)"
    "          ? (ternary) for (loop)")
  "List of feature descriptions shown at startup.")

(defun main ()
  (let ((vars (make-hash-table :test #'equal))
        (funcs (make-hash-table :test #'equal)))
    (format t "Calculator (type 'quit' to exit)~%")
    (dolist (f *features-list*)
      (format t "~a~%" f))
    (loop
      (format t "~%")
      (force-output)
      (let ((input (read-line *standard-input* nil nil)))
        (unless input (return))
        (when (string-equal input "quit") (return))
        (dolist (expr (uiop:split-string input :separator '(#\;)))
          (process-expression (string-trim '(#\Space #\Tab) expr) vars funcs))))))

;;; Entry point for saved executable
(defun calc-toplevel ()
  (main)
  (uiop:quit 0))
