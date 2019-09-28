(defpackage :write-you-a-haskell
  (:use :cl :iterate))
(defpackage :hindley-milner
  (:documentation "holds the symbols of the strongly-typed language i am constructing")
  (:export :variable
           :funcall
           :lambda
           :let
           :quote
           :if
           :binop
           :+
           :-
           :*
           :/
           :def
           :type
           :type-variable
           :type-primitive
           :->
           :forall)
  (:nicknames :hm))
