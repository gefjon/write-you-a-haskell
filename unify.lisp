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

(defgeneric unify (lhs rhs)
  (:documentation "returns a unifying SUBSTITUTION, or throws an error"))

(defmethod unify ((lhs hm:->) (rhs hm:->))
  (with-slots ((lh-in input) (lh-out output)) lhs
    (with-slots ((rh-in input) (rh-out output)) rhs
      (unify (list lh-in lh-out)
             (list rh-in rh-out)))))

(defmethod unify ((lhs hm:type-variable) rhs)
  (bind lhs rhs))

(defmethod unify (lhs (rhs hm:type-variable))
  (bind rhs lhs))

(defmethod unify ((lhs hm:type-primitive) (rhs hm:type-primitive))
  (declare (ignorable lhs rhs))
  (unless (eq lhs rhs)
    (error "cannot unify non-EQ primitive types ~a with ~a" lhs rhs)))

(defmethod unify ((lhs null) (rhs null))
  (declare (ignorable lhs rhs))
  ())

(defmethod unify ((lhs cons) (rhs cons))
  (iter (for lht in lhs)
        (for rht in rhs)
        (with partial-solution = nil)
        (appending (unify (apply-subst partial-solution lht)
                          (apply-subst partial-solution rht)))))

(declaim (ftype (function ((trivial-types:proper-list constraint))
                          substitution)
                solve))
(defun solve (constraints)
  (iter (for cnstrnt in constraints)
        (for lht = (constraint-lhs cnstrnt))
        (for rht = (constraint-rhs cnstrnt))
        (with partial-solution = nil)
        (appending (unify (apply-subst partial-solution lht)
                          (apply-subst partial-solution rht)))))
