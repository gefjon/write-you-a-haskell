(in-package :write-you-a-haskell)

(defparameter *a* (make-type-variable 'a))
(defparameter *b* (make-type-variable 'b))
(defparameter *a->a* (make--> *a* *a*))
(defparameter *a->b* (make--> *a* *b*))
(defparameter *b=boolean* (singleton-subst *b* *boolean*))
(defparameter *a=fixnum* (singleton-subst *a* *fixnum*))
(defparameter *identity-function-scheme* (make-forall (list *a*) (make--> *a* *a*)) "forall a. a -> a")
(defparameter *identity-type-scheme* (make-forall (list *a*) *a*) "forall a. a")
