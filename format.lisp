(defpackage :alimenta.format
  (:use :cl :alexandria :serapeum :fw.lu))
(in-package :alimenta.format)

(defclass document-formatter ()
  ())

(defclass indent-formatter (document-formatter)
  ((%level :initarg :level :accessor level :initform 0)))

(defclass html-formatter (document-formatter)
  ((%level :initarg :level :accessor level :initform 0)))

(defgeneric format-document (formatter stream document)
  (:documentation "Format a document with the given formatter to the given stream"))

(defgeneric format-title (formatter title)
  (:documentation "Format a title according to FORMATTER"))

(defgeneric format-link (formatter link)
  (:documentation "Format a link according to FORMATTER"))

(defgeneric format-paragraph (formatter paragraph)
  (:documentation "Format a paragraph according to FORMATTER"))

(defmethod format-title ((formatter indent-formatter) (title string))
  (format nil "+ Title: ~a" title))

(defmethod format-title ((formatter html-formatter) (title string))
  (format nil "<h~d>~a~2:*</h~d>" (1+ (level formatter)) title))

(defmethod format-link ((formatter indent-formatter) (link string))
  (format nil "  Link: ~a" link))

(defmethod format-link ((formatter html-formatter) (link string))
  (format nil "<a href=\"~a\">~:*~a</a>" link))

(defmethod format-paragraph ((formatter indent-formatter) (paragraph list))
  (format nil "~{  ~a~}" paragraph))

(defmethod format-paragraph ((formatter html-formatter) (paragraph list))
  (format nil "~%~{<p>~a</p>~}" paragraph))


(defmethod format-document (formatter stream (document alimenta::feed-entity))
  (format stream "~&~v,4@t~a~%~v,4@t~a~%"
	  (level formatter) (format-title formatter (alimenta:title document))
	  (level formatter) (format-link formatter (alimenta:link document))))

(defmethod format-document (formatter stream (document alimenta:item))
  (call-next-method)
  (let ((paragraphs (lquery:$ (initialize (alimenta:content document))
			      (children)
			      (text))))
    (format stream "~&~v,4@t~a~2&"
	    (level formatter) (format-paragraph formatter (map 'list #'identity paragraphs)))))

(defmethod format-document ((formatter indent-formatter) stream (document alimenta:feed))
  (call-next-method)
  (incf (level formatter))
  (for:for ((item over document))
    (format-document formatter stream item)))

(defmethod format-document ((formatter html-formatter) stream (document alimenta:feed))
  (let ((ostream (or stream (make-string-output-stream))))
    (unwind-protect
	 (progn (format ostream "~&<html><head></head><body><main>~%")
		(call-next-method formatter ostream document)
		(incf (level formatter))
		(for:for ((item over document))
		  (format ostream "~&<article>~%")
		  (format-document formatter ostream item)
		  (format ostream "~&</article>~%"))
		(format ostream "~&</main></body></html>~%")
		(finish-output ostream)
		(get-output-stream-string ostream))
      (unless stream
	(close ostream)))))
