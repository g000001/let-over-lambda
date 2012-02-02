(cl:in-package :let-over-lambda.internal)
(in-readtable :let-over-lambda)

(def-suite let-over-lambda)

(in-suite let-over-lambda)

(test sharp-macro
  (is (equal '(declare (optimize (speed 3) (safety 0)))
             '#3f)))
