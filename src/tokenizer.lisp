(in-package #:calc)

(defun tokenize (expr)
  (let ((tokens nil) (cur "") (i 0) (len (length expr)))
    (loop while (< i len) do
      (let ((ch (char expr i)))
        (cond
          ;; Whitespace: flush current token
          ((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline))
           (when (> (length cur) 0)
             (push (copy-seq cur) tokens)
             (setf cur ""))
           (incf i))
          ;; Two-char operators: >=, <=, !=
          ((and (or (char= ch (char ">" 0))
                    (char= ch (char "<" 0))
                    (char= ch (char "!" 0)))
                (< (1+ i) len)
                (char= (char expr (1+ i)) (char "=" 0)))
           (when (> (length cur) 0)
             (push (copy-seq cur) tokens)
             (setf cur ""))
           (push (format nil "~c~c" ch (char "=" 0)) tokens)
           (incf i 2))
          ;; Minus sign as part of negative number literal:
          ;; if buffer is empty and next char is digit/period, accumulate as number
          ((and (char= ch #\-)
                (= (length cur) 0)
                (< (1+ i) len)
                (let ((next (char expr (1+ i))))
                  (or (digit-char-p next) (char= next #\.))))
           (setf cur (concatenate 'string cur (string ch)))
           (incf i))
          ;; Operators: + - * / ^ ! ( ) ? = > <
          ((or (char= ch #\+) (char= ch #\-) (char= ch #\*)
               (char= ch #\/) (char= ch #\^) (char= ch #\!)
               (char= ch #\() (char= ch #\))
               (char= ch #\?)
               (char= ch (char "=" 0))
               (char= ch (char ">" 0))
               (char= ch (char "<" 0)))
           (if (and (> (length cur) 0)
                    (char= (char cur 0) #\M)
                    (= (length cur) 1)
                    (or (char= ch #\+) (char= ch #\-)))
               (progn
                 (push (concatenate 'string cur (string ch)) tokens)
                 (setf cur "")
                 (incf i))
               (progn
                 (when (> (length cur) 0)
                   (push (copy-seq cur) tokens)
                   (setf cur ""))
                 (push (string ch) tokens)
                 (incf i))))
          ;; Default: accumulate into current token
          (t
           (setf cur (concatenate 'string cur (string ch)))
           (incf i)))))
    (when (> (length cur) 0)
      (push (copy-seq cur) tokens))
    (nreverse tokens)))
