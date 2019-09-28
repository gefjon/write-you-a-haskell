(in-package :write-you-a-haskell)

(declaim (ftype (function (hm:type-variable t) boolean)
                type-var-occurs-in-p))
(defun type-var-occurs-in-p (tvar within)
  (not (not (member tvar (free-type-variables within)))))

(declaim (ftype (function (hm:type-variable hm:type) substitution)
                bind))
(defun bind (tvar type)
  (cond ((eq tvar type) ())
        ((type-var-occurs-in-p tvar type)
         (error "cannot substitute ~a for infinite type ~a" tvar type))
        ('otherwise (singleton-subst tvar type))))

(defgeneric unify (lhs rhs))

(defmethod unify ((lhs hm:->) (rhs hm:->))
  (let* ((input-subst (unify (->-input lhs) (->-input rhs)))
         (lhs-out (apply-subst input-subst (->-output lhs)))
         (rhs-out (apply-subst input-subst (->-output rhs))))
    (compose input-subst (unify lhs-out rhs-out))))

(defmethod unify ((lhs hm:type-variable) rhs)
  (bind lhs rhs))

(defmethod unify (lhs (rhs hm:type-variable))
  (bind rhs lhs))

(defmethod unify ((lhs hm:type-primitive) (rhs hm:type-primitive))
  (declare (ignorable lhs rhs))
  (unless (eq lhs rhs)
    (error "cannot unify non-EQ primitive types ~a with ~a" lhs rhs)))
