(in-package :write-you-a-haskell)

(gefjon-utils:defclass type-env
    ((alist (trivial-types:association-list hm:variable type-scheme))))

(declaim (ftype (function (type-env hm:variable hm:forall) type-env)
                extend))
(defun extend (env var forall)
  (make-type-env (acons var forall (type-env-alist env))))

(declaim (ftype (function (type-env hm:variable) type-scheme)
                type-env-lookup))
(defun type-env-lookup (env var)
  (let ((assoc (assoc var (type-env-alist env))))
    (unless assoc (error "unbound type-variable ~a in ~a" var env))
    (instantiate (cdr assoc))))

(defparameter *empty-type-env* (make-type-env ()))
