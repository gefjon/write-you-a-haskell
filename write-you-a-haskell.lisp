;;;; this file is for stuff i would type in a repl, but i think i might want to run it more than once

(in-package :write-you-a-haskell)

(defparameter *a->a* (make--> 'a 'a))
(defparameter *a->b* (make--> 'a 'b))
(defparameter *b=boolean* (singleton-subst 'b *boolean*))
(defparameter *a=fixnum* (singleton-subst 'a *fixnum*))
(defparameter *identity-type-scheme* (make-forall (list 'a) 'a) "forall a. a")

(defparameter *compose*
  (hm:lambda a
    (hm:lambda b
      (hm:lambda c
        (hm:funcall a (hm:funcall b c))))))

(defparameter *compose-type-and-constraints*
  (multiple-value-list (infer *compose*)))

(defparameter *compose-type* (first *compose-type-and-constraints*))

(defparameter *compose-constraints* (second *compose-type-and-constraints*))

(defparameter *compose-solution* (solve *compose-constraints*))

(defparameter *substituted-compose-type* (apply-subst *compose-solution* *compose-type*))

(defparameter *identity-function* (make-lambda 'a 'a))

(defparameter *identity-function-type* (infer *identity-function*))
(defparameter *identity-function-type-scheme* (generalize *identity-function-type*))
