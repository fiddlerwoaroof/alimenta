;;;; alimenta.lisp
(declaim (optimize (speed 0) (safety 3) (debug 4)))

(in-package #:alimenta)

(defgeneric -to-feed (doc type &key feed-link)
  (:documentation "Given an xml-document, return a feed object"))

(defgeneric generate-xml (feed feed-type &key partial)
  (:documentation "Given a lisp object representing a feed, return an xml
                   document"))

(defclass feed () 
  ((description :initarg :description :initform nil :accessor description)
   (doc :initarg :doc :initform nil :accessor doc)
   (feed-link :initarg :feed-link :initform nil :accessor feed-link)
   (items :initarg :items :initform nil :accessor items)
   (link :initarg :link :initform nil :accessor link)
   (source-type :initarg :source-type :initform nil :accessor source-type)
   (title :initarg :title :initform nil :accessor title)))

(defclass item ()
  ((author :initarg :author :initform nil :accessor author)
   (content :initarg :content :initform nil :accessor content)
   (date :initarg :date :initform nil :accessor date)
   (doc :initarg :doc :initform nil :accessor doc)
   (id :initarg :id :initform nil :accessor id)
   (link :initarg :link :initform nil :accessor link)
   (links :initform (make-hash-table :test #'equalp) :accessor links)
   (title :initarg :title :initform nil :accessor title)))

(defclass complex-value () ())

(defgeneric primary-value (self)
  (:documentation "Primarily for COMPLEX-VALUES: this should take one and
                   return a useful primary value"))

(defgeneric push-item (feed item)
  (:documentation "Adds an item to the feed"))

(defgeneric make-item (xml-dom doc-type)
  (:documentation "Given an xml document, return an item"))

(defgeneric parse-feed (feed)
  (:documentation "Parse a feed into a lisp object"))

(defgeneric get-items (xml feed-type)
  (:documentation "Given an xml document, extract its items"))

(defmethod primary-value ((self t))
  self)

(define-condition duplicate-link-type (error)
  ((old :reader duplicate-link-type-old :initarg :old)
   (new :reader duplicate-link-type-new :initarg :new))
  (:report (lambda (condition stream)
             (format stream "Item already has link ~s" (duplicate-link-type-old condition)))))


(defmethod generate-xml :around ((feed feed) feed-type &rest r)
  (declare (ignore r))
  (let ((result (call-next-method feed feed-type)))
    (with-slots (items) feed
      (loop for item in items
            do (generate-xml item feed-type :partial result)))
    result))

(defmethod -to-feed :around ((xml-dom plump:node) doc-type &key feed-link)
  "This wraps the particular methods so that _they_ don't have to implement
   item fetching.  NIL passed to the type activates auto-detection"
  (aprog1 (call-next-method xml-dom doc-type :feed-link feed-link)
    (with-slots (doc source-type) it
      (setf doc xml-dom
            source-type doc-type))
    (with-slots (items) it
      (setf
        items (loop for item across (get-items xml-dom doc-type)
                    collect (make-item item doc-type))))))

(defgeneric (setf link) (value self))
(defmethod (setf link) ((value cons) (self item))
  (with-slots (links) self
    (destructuring-bind (type . href) value
      (when (consp href)
        (if (null (cdr href))
          (setf href (car href))
          (error 'type-error "too many arguments")))
      (let ((type-keyword (make-keyword (string-upcase type))))
        (when (slot-boundp self 'links)
          (multiple-value-bind (old-link old-link-p) (gethash type-keyword links) 
            (when old-link-p
              (cerror "Replace Link ~a:~a with ~a:~a" 'duplicate-link-type :old old-link :new href))))
        (setf (gethash type-keyword links) href)))))

(defmethod print-object ((object feed) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title link) object
      (format stream "title: ~s link: ~s"
              (aif title (shorten-link it) "<untitled>")
              (aif link (shorten-link it) "<no link>")))))

(defmethod print-object ((object item) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title link date) object
      (format stream "title: ~s link: ~s date:~s"
              (aif title (shorten-link it) "<untitled>")
              (aif link (shorten-link it) "<no link>")
              (aif date it "<no date>")))))


(defun detect-feed-type (xml-dom)
  (let ((root-node-name (make-keyword (string-upcase
                                        ($ (inline xml-dom) (children)
                                           (map #'tag-name) (node))))))
    (setf type
          (case root-node-name
            ((:feed) :atom)
            (t root-node-name)))))


;(defun generate-xml (feed &key (feed-type :rss))
;  (%generate-xml feed feed-type))

(defun to-feed (doc &key type feed-link)
  "Makes an instance of feed from the given document.  Specialize to-feed with
   an equal-specializer on type with an appropriate symbol to implement a new
   sort of feed."
  (unless type
    (setf type (detect-feed-type doc)))
  (-to-feed doc type :feed-link feed-link))


;(defun -get-items (feed xml-dom &key type)
;  (with-slots (items) feed
;    (loop for item across (get-items xml-dom type)
;          do (push (make-item xml-dom type) items)
;          finally (return items)))) 

(defun make-feed (&key title link items feed-link description)
  (make-instance 'feed
                 :description description
                 :feed-link feed-link
                 :items items
                 :link link
                 :title title))

(let ((n 0))
  (defun next-id ()
    (incf n)))

(defun add-item-to-feed (feed &key title (next-id #'next-id) date link content)
  (alet (make-instance 'item :title title :date date :link link :content content)
    (with-slots (id) it
      (setf id (funcall next-id it)))
    (push-item feed it)
    (values feed it)))

(defun shorten-link (link)
  (let ((link (cl-ppcre:regex-replace "^https?:" link "")))
    (subseq link 0 (min 30 (length link)))))

(defmethod push-item ((feed feed) (item item))
  (push item (slot-value feed 'items)))

(deftest push-item ()
  (let ((feed (make-instance 'feed))
        (item (make-instance 'item)))
    (with-slots (items) feed
      ;(should signal error (push-item feed 2))
      (should be eql item
              (progn
                (push-item feed item)
                (car items))))))


;; vim: set foldmethod=marker:
