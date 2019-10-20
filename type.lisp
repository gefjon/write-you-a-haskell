
(in-package :write-you-a-haskell)

(defenum hm:type
    symbol
  (hm:type-primitive
   ((name symbol)))
  (hm:->
   ((input hm:type) (output hm:type))))

(declaim (type hm:type-primitive *fixnum* *boolean*))
(defparameter *fixnum* (hm:type-primitive fixnum))
(defparameter *boolean* (hm:type-primitive boolean))

(declaim (ftype (function (&optional (or string symbol)) symbol)))
(defun new-type-variable (&optional (name "TYPE-VAR-"))
  (gensym (gefjon-utils:coerce-to-string name)))
