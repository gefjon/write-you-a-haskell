(in-package :write-you-a-haskell)

;; as with HM:DEF and EXPR, HM:FORALL is not a member of HM:TYPE,
;; because it cannot appear in the type language
;; i think later languages in the book extend this type with other
;; members. either way, it looks a bit less weird in haskell, and i'm
;; including the type alias for consistently with the book.
(defenum type-scheme
    (hm:forall
     ((bindings (trivial-types:proper-list symbol))
      (body hm:type))))

(declaim (ftype (function (hm:forall symbol) boolean)
                forall-closes-over-p))
(defun forall-closes-over-p (forall var)
  (member var (forall-bindings forall)))
