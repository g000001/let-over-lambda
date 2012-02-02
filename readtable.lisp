;;;; readtable.lisp

(cl:in-package :let-over-lambda.internal)
(in-readtable :common-lisp)

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))


(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                (cdr pointer)
                pattern))
        (if (null pointer)
          (return)))
      (coerce
        (nreverse
          (nthcdr (length pattern) output))
        'string))))


(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
           (segment-reader stream
                           (read-char stream)
                           1)))
      ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
           (segment-reader stream
                           (read-char stream)
                           2)))
      (t (error "Unknown #~~ mode character")))))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
        (get-macro-character #\`) stream nil)))


(defreadtable :let-over-lambda  (:merge :standard)
  (:dispatch-macro-char #\# #\" #'|#"-reader|)
  (:dispatch-macro-char #\# #\> #'|#>-reader|)
  (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\f
                        (lambda (stream sub-char numarg)
                          (declare (ignore stream sub-char))
                          (setq numarg (or numarg 3))
                          (unless (<= numarg 3)
                            (error "Bad value for #f: ~a" numarg))
                          `(declare (optimize (speed ,numarg)
                                              (safety ,(- 3 numarg))))))
  (:case :upcase))
