(in-package :write-you-a-haskell)

(deftype substitution ()
  '(trivial-types:association-list symbol hm:type))

(defparameter *empty-substitution* ())

(declaim (ftype (function (symbol hm:type) substitution)
                singleton-subst))
(defun singleton-subst (tvar type)
  (acons tvar type ()))

(declaim (ftype (function (substitution &rest substitution) substitution)
                compose))
(defun compose (enclosing &rest inner)
  (apply #'concatenate 'list enclosing inner))

(defgeneric apply-subst (substitution target))

;;; implementations for HM:TYPE
(defmethod apply-subst (substitution (target symbol))
  "if TARGET is bound in SUBSTITUTION, use its type; otherwise, return TARGET"
  (or (cdr (assoc target substitution)) target))

(defmethod apply-subst (substitution (target hm:type-primitive))
  (declare (ignorable substitution))
  "primitive types ignore substitution"
  target)

(defmethod apply-subst (substitution (target hm:->))
  (flet ((recurse (on) (apply-subst substitution on)))
    (make--> (recurse ( ->-input target))
             (recurse (->-output target)))))

;;; implementations for TYPE-SCHEME

(defmethod apply-subst (substitution (target hm:forall))
  (flet ((closed-over-p (type-var)
           (forall-closes-over-p target type-var)))
    (make-forall (forall-bindings target) (apply-subst (remove-if #'closed-over-p substitution
                                                                  :key #'car)
                                                       (forall-body target)))))

;;; implementations for lists

(defmethod apply-subst (substitution (target cons))
  (iter (for el in target)
        (collecting (apply-subst substitution el))))

(defmethod apply-subst (substitution (target null))
  (declare (ignorable substitution target))
  ())

;;; implementation for TYPE-ENV

(defmethod apply-subst (substitution (target type-env))
  (flet ((substitute-alist-cell (cell)
           (destructuring-bind (key . value) cell
             (cons key (apply-subst substitution value)))))
    (make-type-env (mapcar #'substitute-alist-cell (type-env-alist target)))))
