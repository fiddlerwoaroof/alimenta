;;;; alimenta.asd

(asdf:defsystem #:alimenta
  :description "A little library to discover, fetch, parse and generate RSS feeds"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:chronicity
               #:drakma
               #:for
               #:fwoar.lisputils
               #:lquery
               #:plump
               #:serapeum
               #:should-test
               #:spinneret
               #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "collections")
               (:file "collections-for")
               #+sbcl (:file "collections-sbcl-iterators")

               (:file "alimenta")  
               (:file "data-class")
               (:file "date-handling")
               (:file "atom")  
               (:file "rss")  
               (:file "fetching")
               (:file "discovery")))


;; vim: set ft=lisp:
