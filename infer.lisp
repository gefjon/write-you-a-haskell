(in-package :write-you-a-haskell)

(gefjon-utils:defstruct constraint
  ((lhs hm:type)
   (rhs hm:type)))

(defmethod apply-subst (substitution (target constraint))
  (with-slots (lhs rhs) target
    (make-constraint (apply-subst substitution lhs)
                     (apply-subst substitution rhs))))

(defgeneric infer (expr &optional type-env)
  (:documentation "returns (VALUES HM:TYPE (TRIVIAL-TYPES:PROPER-LIST CONSTRAINT))")
  (:method :around (expr &optional (type-env *empty-type-env*))
           (call-next-method expr type-env)))

(defmethod infer ((expr hm:variable) &optional type-env)
  (values (type-env-lookup type-env expr)
          ()))

(defmethod infer ((expr hm:lambda) &optional type-env)
  (let* ((arg-type (new-type-variable (lambda-binding expr)))
         (function-env (extend type-env (lambda-binding expr) (make-forall () arg-type))))
    (multiple-value-bind (return-type constraints) (infer (lambda-body expr) function-env)
      (values (make--> arg-type return-type)
              constraints))))

(defmethod infer ((expr hm:funcall) &optional type-env)
  (multiple-value-bind (function-type function-constraints) (infer (funcall-function expr) type-env)
    (multiple-value-bind (arg-type arg-constraints) (infer (funcall-arg expr) type-env)
      (let* ((return-type (new-type-variable))
             (arrow-constraint (make-constraint function-type
                                                (make--> arg-type return-type))))
        (values return-type
                (concatenate 'list function-constraints arg-constraints (list arrow-constraint)))))))

(defmethod infer ((expr hm:let) &optional type-env)
  (multiple-value-bind (bind-type bind-constraint) (infer (let-value expr) type-env)
    (let* ((bind-scheme (generalize type-env bind-type))
           (local-env (extend type-env (let-binding expr) bind-scheme)))
      (multiple-value-bind (body-type body-constraint) (infer (let-body expr) local-env)
        (values body-type
                (concatenate 'list bind-constraint body-constraint))))))

(defmethod infer ((expr hm:if) &optional type-env)
  (multiple-value-bind (pred-type pred-constraint) (infer (if-predicate expr) type-env)
    (multiple-value-bind (then-type then-constraint) (infer (if-then-case expr) type-env)
      (multiple-value-bind (else-type else-constraint) (infer (if-else-case expr) type-env)
        (let ((return-constraint (make-constraint then-type else-type))
              (pred-bool-constraint (make-constraint pred-type *boolean*)))
          (values then-type
                  (concatenate 'list (list return-constraint pred-bool-constraint)
                               else-constraint
                               then-constraint
                               pred-constraint)))))))

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

(defmethod infer ((expr hm:binop) &optional type-env)
  (multiple-value-bind (lh-type lh-constr) (infer (binop-lhs expr) type-env)
    (multiple-value-bind (rh-type rh-constr) (infer (binop-rhs expr) type-env)
      (let* ((return-type (new-type-variable))
             (arrow-type (make--> lh-type (make--> rh-type return-type)))
             (arrow-constr (make-constraint arrow-type (op-type (binop-op expr)))))
        (values return-type
                (concatenate 'list lh-constr rh-constr (list arrow-constr)))))))

(defmethod infer ((expr hm:quote) &optional type-env)
  (declare (ignorable type-env))
  (values (etypecase (quote-it expr)
            (fixnum *fixnum*)
            (boolean *boolean*))
          ()))
