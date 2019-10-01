(in-package :write-you-a-haskell)

(defgeneric infer (type-env expr)
  (:documentation "returns (VALUES SUBSTITUTION HM:TYPE)"))

(defmethod infer (type-env (expr hm:variable))
  (values *empty-substitution*
          (type-env-lookup type-env expr)))

(defmethod infer (type-env (expr hm:funcall))
  (multiple-value-bind (function-subst function-type) (infer type-env (funcall-function expr))
    (multiple-value-bind (arg-subst arg-type) (infer (apply-subst function-subst type-env) (funcall-arg expr))
      (let* ((return-type (new-type-variable))
             (unif-subst (unify (apply-subst arg-subst function-type)
                                (make--> arg-type return-type))))
        (values (compose unif-subst arg-subst function-subst)
                (apply-subst unif-subst return-type))))))

(defmethod infer (type-env (expr hm:lambda))
  (let* ((arg-type (new-type-variable))
         (function-env (extend type-env (lambda-binding expr) (make-forall () arg-type))))
    (multiple-value-bind (subst return-type) (infer function-env (lambda-body expr))
      (values subst (apply-subst subst (make--> arg-type return-type))))))

(defmethod infer (type-env (expr hm:let))
  (multiple-value-bind (bind-subst bind-type) (infer type-env (let-value expr))
    (let* ((substituted-enclosing-env (apply-subst bind-subst type-env))
           (bind-scheme (generalize substituted-enclosing-env bind-type))
           (local-env (extend substituted-enclosing-env (let-binding expr) bind-scheme)))
      (multiple-value-bind (body-subst body-type) (infer local-env (let-body expr))
        (values (compose body-subst bind-subst) body-type)))))

(defmethod infer (type-env (expr hm:if))
  (multiple-value-bind (pred-subst pred-type) (infer type-env (if-predicate expr))
    (multiple-value-bind (then-subst then-type) (infer type-env (if-then-case expr))
      (multiple-value-bind (else-subst else-type) (infer type-env (if-else-case expr))
        (let ((return-subst (unify then-type else-type))
              (pred-bool-subst (unify pred-type *boolean*)))
          (values (compose return-subst
                           pred-bool-subst
                           else-subst
                           then-subst
                           pred-subst)
                  (apply-subst pred-bool-subst then-type)))))))

(declaim (ftype (function (operator) hm:->)
                op-type))
(defun op-type (operator)
  (let ((fixnum->fixnum->fixnum (make--> *fixnum* (make--> *fixnum* *fixnum*)))
        (fixnum->fixnum->boolean (make--> *fixnum* (make--> *fixnum* *boolean*))))
    (ecase operator
      (hm:+ fixnum->fixnum->fixnum)
      (hm:- fixnum->fixnum->fixnum)
      (hm:* fixnum->fixnum->fixnum)
      (hm:/ fixnum->fixnum->fixnum)
      (hm:= fixnum->fixnum->boolean))))

(defmethod infer (type-env (expr hm:binop))
  (multiple-value-bind (lh-subst lh-type) (infer type-env (binop-lhs expr))
    (multiple-value-bind (rh-subst rh-type) (infer type-env (binop-rhs expr))
      (let* ((return-type (new-type-variable))
             (arrow-type (make--> lh-type (make--> rh-type return-type)))
             (arrow-subst (unify arrow-type (op-type (binop-op expr)))))
        (values (compose lh-subst rh-subst arrow-subst)
                (apply-subst arrow-subst return-type))))))

(defmethod infer (type-env (expr hm:quote))
  (values *empty-substitution*
   (etypecase (quote-it expr)
     (fixnum *fixnum*)
     (boolean *boolean*))))
