(in-package :write-you-a-haskell)

(defenum hm:type
    (hm:type-variable
     ((name symbol)))
  (hm:type-primitive
   ((name symbol)))
  (hm:->
   ((input hm:type) (output hm:type))))

(declaim (type hm:type-primitive *fixnum* *boolean*))
(defparameter *fixnum* (make-type-primitive 'fixnum))
(defparameter *boolean* (make-type-primitive 'boolean))

(declaim (ftype (function (&optional (or string symbol hm:type-variable)) hm:type-variable)))
(defun new-type-variable (&optional (name "type-var"))
  (let* ((name-sym (typecase name
                     (hm:type-variable (type-variable-name name))
                     (hm:variable (variable-name name))
                     (t name)))
         (name-string (if (typep name-sym 'symbol) (symbol-name name-sym) name-sym)))
    (make-type-variable (gensym name-string))))
