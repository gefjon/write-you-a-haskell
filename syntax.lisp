(in-package :write-you-a-haskell)

(deftype literal ()
  '(or fixnum boolean))

(deftype operator ()
  '(member hm:+ hm:- hm:* hm:/ hm:=))

(defenum expr
    (hm:variable ((name symbol)))
  (hm:funcall ((function expr)
               (arg expr)))
  (hm:lambda ((binding hm:variable)
              (body expr)))
  (hm:let ((binding hm:variable)
           (value expr)
           (body expr)))
  (hm:quote ((it literal)))
  (hm:if ((predicate expr)
          (then-case expr)
          (else-case expr)))
  (hm:binop ((op operator)
             (lhs expr)
             (rhs expr))))

;; note that HM:DEF is not a member of EXPR, because top-level
;; definitions are not part of the expression language
(defexpr hm:def
    ((binding symbol)
     (value expr)))

(gefjon-utils:defstruct program
  ((definitions (trivial-types:proper-list hm:def))
   (entry expr)))
