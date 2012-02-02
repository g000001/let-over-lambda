;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :let-over-lambda
  (:nicknames :lol)
  (:use)
  (:export :group
           :mkstr
           :symb
           :flatten
           :defmacro!
           :nlet
           :nif
           :cyclic-p
           :defunits
           :nlet-tail
           :cxr
           :cxr-inline-thresh
           :dlambda
           :alambda
           :alet
           :sublet
           :sublet*
           :pandoriclet-get
           :pandoriclet-set
           :get-pandoric
           :with-pandoric
           :pandoric-hotpatch
           :pandoric-recode
           :plambda
           :defpan
           :pandoric-eval
           :fast-progn
           :safe-progn
           :dis
           ))

(defpackage :let-over-lambda.internal
  (:use :let-over-lambda :cl :named-readtables :fiveam))
