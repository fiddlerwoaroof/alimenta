;;;; package.lisp

(defpackage :alimenta.render
  (:use :cl )
  (:export #:render-feed #:render-item #:add-rendered-item))

(defpackage #:data-class
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora)
  (:export #:define-data-class))

(defpackage #:alimenta2
  (:use #:cl #:alexandria #:serapeum #:should-test))

(defpackage #:alimenta
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora #:collection-class)
  (:export #:to-feed #:generate-xml #:feed #:title #:link #:items #:feed-link #:feed-type
           #:doc #:source-type #:id #:date #:content #:item #:description
           #:%generate-xml #:%to-feed #:get-items #:make-item #:complex-value
           #:primary-value #:render #:author #:content-el #:feed-type-unsupported
           #:pop-token #:filter-feed #:feed-entity
           #:transform))

(defpackage #:alimenta.html
  (:use #:cl)
  (:export #:html-renderer))

(defpackage #:alimenta.rss
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora #:alimenta #:data-class)
  (:export #:language #:copyright #:managing-editor #:webmaster
           #:publication-date #:last-build-date #:categories #:generator #:docs
           #:cloud #:ttl #:image #:rating #:text-input #:skip-hours #:skip-days
           #:rss-feed #:rss-item #:category #:domain))

(defpackage #:alimenta.atom
  (:use #:cl #:should-test #:lquery #:plump #:alexandria #:anaphora #:alimenta))

(defpackage #:alimenta.discover
  (:use #:cl #:alimenta #:alexandria #:anaphora #:lquery)
  (:export #:discover-feed))

(defpackage #:alimenta.pull-feed
  (:use #:cl #:alimenta #:alexandria #:anaphora #:lquery)
  (:export #:pull-feed #:fetch-doc-from-url #:fetch-feed-from-url
           #:fetch-error #:feed-ambiguous #:no-feed #:with-user-agent
	         #:skip-feed))

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
