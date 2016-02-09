;;;; package.lisp

(defpackage #:alimenta
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora)
  (:export #:to-feed #:generate-xml
           #:feed #:title #:link #:items #:feed-link #:doc #:source-type #:id #:date #:content
           #:item #:description #:%generate-xml #:%to-feed #:%get-items #:make-item))

(defpackage #:alimenta.rss
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora #:alimenta))

(defpackage #:alimenta.atom
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora #:alimenta))

(defpackage #:alimenta.discover
  (:use #:cl #:alimenta #:alexandria #:anaphora #:lquery)
  (:export #:discover-feed))

(defpackage #:alimenta.pull-feed 
  (:use #:cl #:alimenta #:alexandria #:anaphora #:lquery)
  (:export #:pull-feed)) 

(defmethod asdf:perform ((o asdf:test-op) (s (eql (asdf:find-system :alimenta))))
  (asdf:load-system :alimenta)
  (st:test :package :alimenta)
  t)

(defpackage #:alimenta.test-runner
  (:use #:cl #:alimenta #:alimenta.atom #:alimenta.discover #:alimenta.pull-feed))

(in-package :alimenta.test-runner)

(defclass xunit-test (asdf:test-op) ())

(defmethod asdf:perform ((o asdf:test-op) (s (eql (asdf:find-system :alimenta))))
  (asdf:load-system :alimenta)
  (or (st:test :package :alimenta)
      (st:test :package :alimenta.atom)
      (st:test :package :alimenta.discover)
      (st:test :package :alimenta.pull-feed)))
