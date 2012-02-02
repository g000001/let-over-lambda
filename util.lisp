(cl:in-package :let-over-lambda.internal)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons
                                (subseq source 0 n)
                                acc))
                     (nreverse
                      (cons source acc))))))
      (if source (rec source nil) nil)))

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s)) ))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))) )

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))


  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil)))


  (defmacro defmacro/g! (name args &rest body)
    (let ((syms (remove-duplicates
                 (remove-if-not #'g!-symbol-p
                                (flatten body) ))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2 ))))
                syms )
           ,@body ))))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2) )))


(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)) )
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body) ))))

(defun segment-reader (stream ch n)
  (if (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream) ))
          ((char= ch curr))
        (push curr chars) )
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1)) ))))

(defmacro! match-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str )))


(defmacro! subst-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))
