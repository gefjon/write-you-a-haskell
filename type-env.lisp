(in-package :write-you-a-haskell)

(gefjon-utils:defclass type-env
    ((alist (trivial-types:association-list symbol type-scheme))))

(declaim (ftype (function (type-env symbol hm:forall) type-env)
                extend))
(defun extend (env var forall)
  (make-type-env (acons var forall (type-env-alist env))))
