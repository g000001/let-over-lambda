;;;; let-over-lambda.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :let-over-lambda
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "util")
               (:file "readtable")
               (:file "let-over-lambda")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :let-over-lambda))))
  (load-system :let-over-lambda)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :let-over-lambda.internal :let-over-lambda))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
