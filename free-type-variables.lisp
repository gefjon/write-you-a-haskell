(in-package :write-you-a-haskell)

(defgeneric free-type-variables (within))

;;; implementations for HM:TYPE

(defmethod free-type-variables ((within hm:type-variable))
  (list within))

(defmethod free-type-variables ((within hm:type-primitive))
  (declare (ignorable within))
  "primitives have no free vars"
  ())

(defmethod free-type-variables ((within hm:->))
  (union (free-type-variables (->-input within))
         (free-type-variables (->-output within))))

;;; implementations for TYPE-SCHEME

(defmethod free-type-variables ((within hm:forall))
  (flet ((closed-over-p (type-var)
           (forall-closes-over-p within type-var)))
    (remove-if #'closed-over-p (free-type-variables (forall-body within)))))

;;; implementations for lists

(defmethod free-type-variables ((within cons))
  (iter (for el in within)
        (unioning (free-type-variables el))))

(defmethod free-type-variables ((within null))
  (declare (ignorable within))
  ())

;;; implementation for TYPE-ENV

(defmethod free-type-variables ((within type-env))
  (iter (for (key . value) in (type-env-alist within))
        (unioning (free-type-variables value))))
