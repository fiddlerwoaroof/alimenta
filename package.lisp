;;;; package.lisp

(defpackage #:alimenta
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora)
  (:export #:to-feed #:generate-xml
           #:feed #:title #:link #:items #:feed-link #:doc #:source-type #:id #:date #:content
           #:item))

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
