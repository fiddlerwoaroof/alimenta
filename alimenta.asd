;;;; alimenta.asd

(asdf:defsystem #:alimenta
  :description "A little library to discover, fetch, parse and generate RSS feeds"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:plump
               #:lquery
               #:should-test
               #:alexandria
               #:anaphora
               #:drakma)
  :serial t
  :components ((:file "package")
               (:file "alimenta")  
               (:file "fetching")
               (:file "discovery")))


;; vim: set ft=lisp: