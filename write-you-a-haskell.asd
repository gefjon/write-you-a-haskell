;;;; write-you-a-haskell.asd

(asdf:defsystem :write-you-a-haskell
  :description "a hindley-milner typed language i am building with support from the book Write You a Haskell"
  :author "gefjon <arthur@goldman-tribe.org>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:iterate
                :alexandria
                :gefjon-utils
                :trivial-types)
  :serial t
  :components ((:file "package")
               (:file "defexpr")
               (:file "syntax")
               (:file "type")
               (:file "type-scheme")
               (:file "type-env")
               (:file "substitute")
               (:file "free-type-variables")
               (:file "unify")
               (:file "instantiate-generalize")
               (:file "infer")
               (:file "write-you-a-haskell")))
