(in-package :write-you-a-haskell)

;; as with HM:DEF and EXPR, HM:FORALL is not a member of HM:TYPE,
;; because it cannot appear in the type language
(defexpr hm:forall
    ((bindings (trivial-types:proper-list hm:type-variable))
     (body hm:type)))

;; i think later languages in the book extend this type with other
;; members. either way, it looks a bit less weird in haskell, and i'm
;; including the type alias for consistently with the book.
(deftype type-scheme ()
  'hm:forall)

(declaim (ftype (function (hm:forall hm:type-variable) boolean)
                forall-closes-over-p))
(defun forall-closes-over-p (forall var)
  (member var (forall-bindings forall)))
