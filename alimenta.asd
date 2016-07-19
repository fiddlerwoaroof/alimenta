;;;; alimenta.asd

(asdf:defsystem #:alimenta
  :description "A little library to discover, fetch, parse and generate RSS feeds"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:plump
               #:lquery
               #:should-test
               #:alexandria
               #:serapeum
               #:anaphora
               #:chronicity
               #:fwoar.lisputils
               #:split-sequence
               #:drakma)
  :serial t
  :components ((:file "package")
               (:file "alimenta")  
               (:file "data-class")
               (:file "date-handling")
               (:file "atom")  
               (:file "rss")  
               (:file "fetching")
               (:file "discovery")))


;; vim: set ft=lisp:
