(in-package #:calc)


(defun process-expression (tr vars funcs)
  (when (> (length tr) 0)
    (handler-case
        (cond
          ((char= (char tr 0) #\#) nil)
          ((let ((eq (position (char "=" 0) tr :from-end t))
                 (par (position (char "(" 0) tr)))
             (and eq
                  (not par)
                  (let ((lhs (string-trim '(#\Space #\Tab) (subseq tr 0 eq))))
                    (and (> (length lhs) 0)
                         (every #'alpha-char-p lhs)
                         (> eq 0)
                         (< (1+ eq) (length tr))))))
           (let* ((eq (position (char "=" 0) tr))
                  (vn (string-trim '(#\Space #\Tab) (subseq tr 0 eq)))
                  (r (eval-rpn (subseq tr (1+ eq)) vars funcs)))
             (when (and (> (length vn) 0) (every #'alpha-char-p vn))
               (setf (gethash vn vars) r)
               (format t "= ~a~%" r))))
          ;; defmacro
          ((let ((pos (search "defmacro " tr :test #'string-equal)))
             (and pos (= pos 0)))
           (let* ((parts (uiop:split-string tr :separator '(#\Space #\Tab)))
                  (raw-name (second parts))
                  (paren-in-name (when raw-name (position #\( raw-name)))
                  (name (if paren-in-name
                            (subseq raw-name 0 paren-in-name)
                            raw-name))
                  (arglist-start (position #\( tr))
                  (arglist-end (when arglist-start
                                 (position #\) tr :start arglist-start)))
                  (args-str (when arglist-end
                              (string-trim '(#\Space #\Tab)
                                           (subseq tr (1+ arglist-start) arglist-end))))
                  (args (when (and args-str (> (length args-str) 0))
          (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                  (uiop:split-string args-str :separator '(#\, #\Space)))))
                  (body (if arglist-end
                            (subseq tr (1+ arglist-end))
                            (format nil "~{~A~^ ~}" (cddr parts)))))
             (when name
               (setf (gethash (string-upcase name) *macros*)
                     (list :args (or args nil) :body body))
               (format t "Defined macro: ~a~%" name))))
          ;; defun
          ((let ((pos (search "defun " tr :test #'string-equal)))
             (and pos (= pos 0)))
           (let* ((parts (uiop:split-string tr :separator '(#\Space #\Tab)))
                  (raw-name (second parts))
                  ;; Strip "(params)" from the name so lookups match calls
                  (name (if raw-name
                            (let ((p (position #\( raw-name)))
                              (if p (subseq raw-name 0 p) raw-name))))
                  (arglist-start (position #\( tr))
                  (arglist-end (position #\) tr :start arglist-start))
                  (body-start (1+ arglist-end))
                  (args-str (subseq tr (1+ arglist-start) arglist-end))
                  (parsed-args (when (> (length args-str) 0)
                                 (mapcar #'string-trim
                                         (make-list (length (uiop:split-string args-str :separator '(#\, #\Space)))
                                                    :initial-element '(#\Space #\Tab))
                                         (uiop:split-string args-str :separator '(#\, #\Space)))))
                  (args parsed-args)
                  (body (subseq tr body-start)))
             (when (and name args)
               (setf (gethash (string-upcase name) funcs)
                     (list :args args :body body))
               (format t "Defined function: ~a~%" name))))
          ;; Function call
          ((let ((func (gethash (string-upcase (first (uiop:split-string tr :separator '(#\Space #\Tab #\()))) funcs)))
             (when func
               (let* ((args-str (subseq tr (1+ (position #\( tr))))
                      (args-str (subseq args-str 0 (position #\) args-str)))
                      (arg-vals (mapcar #'eval-rpn
                                        (uiop:split-string args-str :separator '(#\,))
                                        (make-list (length (uiop:split-string args-str :separator '(#\,))) :initial-element vars)
                                        (make-list (length (uiop:split-string args-str :separator '(#\,))) :initial-element funcs)))
                      (arg-names (getf func :args))
                      (body (getf func :body)))
                 (dolist (pair (mapcar #'cons arg-names arg-vals))
                   (setf (gethash (string-upcase (car pair)) vars) (cdr pair)))
                 (let ((result (eval-rpn body vars funcs)))
                   (format t "= ~a~%" result)
                   (dolist (pn arg-names) (remhash (string-upcase pn) vars))
                   t)))))
          (t (let ((r (eval-rpn tr vars funcs)))
               (format t "= ~a~%" r))))
      (error (c)
        (format t "Error: ~a~%" c)))))


