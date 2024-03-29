(in-package :write-you-a-haskell)

(declaim (ftype (function (type-scheme) hm:type)
                instantiate))
(defun instantiate (scheme)
  ;; currently, all TYPE-SCHEMEs are HM:FORALLs, so this function
  ;; assumes that its argument is a HM:FORALL
  (flet ((substitute-plist-cell (type-var) (cons type-var
                                                 (new-type-variable type-var))))
    (apply-subst (mapcar #'substitute-plist-cell (forall-bindings scheme))
                 (forall-body scheme))))

(declaim (ftype (function (hm:type &optional type-env) type-scheme)
                generalize))
(defun generalize (type &optional (env *empty-type-env*))
  (make-forall (set-difference (free-type-variables type)
                               (free-type-variables env))
               type))
