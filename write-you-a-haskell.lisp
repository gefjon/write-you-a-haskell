;;;; this file is for stuff i would type in a repl, but i think i might want to run it more than once

(in-package :write-you-a-haskell)

(defparameter *var-a* (make-variable 'a))
(defparameter *var-b* (make-variable 'b))
(defparameter *var-c* (make-variable 'c))
(defparameter *type-a* (make-type-variable 'a))
(defparameter *type-b* (make-type-variable 'b))
(defparameter *a->a* (make--> *type-a* *type-a*))
(defparameter *a->b* (make--> *type-a* *type-b*))
(defparameter *b=boolean* (singleton-subst *type-b* *boolean*))
(defparameter *a=fixnum* (singleton-subst *type-a* *fixnum*))
(defparameter *identity-function-scheme* (make-forall (list *type-a*) (make--> *type-a* *type-a*)) "forall a. a -> a")
(defparameter *identity-type-scheme* (make-forall (list *type-a*) *type-a*) "forall a. a")

(defparameter *compose*
  (make-lambda *var-a*
               (make-lambda *var-b*
                            (make-lambda *var-c*
                                         (make-funcall *var-a* (make-funcall *var-b* *var-c*))))))

(defparameter *compose-type-and-constraints*
  (multiple-value-bind (type constraints) (infer *compose*)
    (list type constraints)))

(defparameter *compose-type* (first *compose-type-and-constraints*))

(defparameter *compose-constraints* (second *compose-type-and-constraints*))

(defparameter *compose-solution* (solve *compose-constraints*))

(defparameter *substituted-compose-type* (apply-subst *compose-solution* *compose-type*))
