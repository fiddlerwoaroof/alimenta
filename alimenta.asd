;;;; alimenta.asd
(in-package :asdf-user)

(defsystem #:alimenta
  :description "A little library to discover, fetch, parse and generate RSS feeds"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:chronicity
               #:drakma
               #:for
               #:fwoar-lisputils
               #:collection-class
               #:lquery
               #:plump
               #:serapeum
               #:should-test
               #:spinneret
               #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "alimenta")
               (:file "data-class")
               (:file "render-protocol")
               (:file "atom")
               (:file "rss")
               (:file "fetching")
               (:file "discovery")))

(defsystem :alimenta/patmatch
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:alimenta
               #:fwoar-lisputils)
  :serial t
  :components ((:file "pattern-matcher")))


;; vim: set ft=lisp:
