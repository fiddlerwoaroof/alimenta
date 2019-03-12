;;;; alimenta.lisp -*- tab-width: 8; -*-
;; (declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:alimenta)

(defclass feed-entity ()
  ((title :initarg :title :initform nil :accessor title)  
   (link :initarg :link :initform nil :accessor link)
   (doc :initarg :doc :initform nil :accessor doc)))

(defgeneric belongs-to (feed-entity)
  (:documentation "Returns the person responsible for this feed item"))

(define-condition feed-type-unsupported (error)
  ((%type :initarg :type :reader feed-type)
   (%feed-link :initarg :feed-link :reader feed-link)))

(defgeneric -to-feed (doc type &key feed-link)
  (:documentation "Given an xml-document, return a feed object")
  (:method (doc type &key feed-link)
    (error 'feed-type-unsupported :type type :feed-link feed-link)))

(defgeneric render (object renderer)
  (:documentation "Given a lisp object representing a feed, return it rendered
                   to the specified format"))

(defgeneric generate-xml (feed feed-type &key partial)
  (:documentation "Given a lisp object representing a feed, return an xml
                   document"))

(defgeneric content-el (entity)
  (:documentation "Return the element that contains the item's content"))

(defclass item (feed-entity)
  ((author :initarg :author :initform nil :accessor author)
   (content :initarg :content :initform nil :accessor content)
   (date :initarg :date :initform nil :accessor date)
   (id :initarg :id :initform nil :accessor id)
   (links :initform (make-hash-table :test #'equalp) :accessor links)))

(collection-class:define-collection (feed item) (feed-entity) 
  ((description :initarg :description :initform nil :accessor description)
   (feed-link :initarg :feed-link :initform nil :accessor feed-link)
   (source-type :initarg :source-type :initform nil :accessor source-type)))

(defmethod render ((feed feed) renderer)
  (let ((doc (alimenta.render:render-feed feed renderer)))
    (for:for ((item over feed))
      (setf doc
            (alimenta.render:add-rendered-item doc
                                               (alimenta.render:render-item item feed renderer)
                                               renderer)))
    doc))

(defmethod (setf feed-link) ((value string) (feed feed))
  (setf (slot-value feed 'feed-link)
        (puri:parse-uri value)))

(defmethod initialize-instance :after ((feed feed) &key feed-link)
  (when feed-link
    (setf (feed-link feed) (puri:parse-uri feed-link))))

(defmethod belongs-to ((item item))
  (author item))

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
    (with-accessors ((items items)) feed
      (loop for item in items
         do (generate-xml item feed-type :partial result)))
    result))

(defmethod -to-feed ((doc stream) doc-type &key feed-link)
  (-to-feed (plump:parse doc)
            doc-type
            :feed-link feed-link))

(defmethod -to-feed ((doc string) doc-type &key feed-link)
  (-to-feed (plump:parse doc)
            doc-type
            :feed-link feed-link))

(defmethod -to-feed :around ((xml-dom plump:node) doc-type &key feed-link)
  "This wraps the particular methods so that _they_ don't have to implement
   item fetching.  NIL passed to the type activates auto-detection"
  (aprog1 (call-next-method xml-dom doc-type :feed-link feed-link)
    (with-slots (doc source-type) it
      (setf doc xml-dom
            source-type doc-type))
    (setf
     (items it)
     (loop for item across (get-items xml-dom doc-type)
        collect (make-item item doc-type)))))

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
    (case root-node-name
      ((:feed) :atom)
      (t root-node-name))))

(defgeneric get-random-item (feed)
  (:method ((feed feed))
    (let* ((items (copy-seq (items feed)))
           (num-items (length items)))
      (elt items
           (random num-items)))))

(defgeneric get-latest-item (feed)
  (:method ((feed feed))
    (let ((items (copy-seq (items feed))))
      (car (sort items
                 #'local-time:timestamp>
                 :key #'date)))))

;;(defun generate-xml (feed &key (feed-type :rss))
;;  (%generate-xml feed feed-type))

(defun to-feed (doc &key type feed-link)
  "Makes an instance of feed from the given document.  Specialize to-feed with
   an equal-specializer on type with an appropriate symbol to implement a new
   sort of feed."
  (unless type
    (setf type (detect-feed-type doc)))
  (-to-feed doc type :feed-link feed-link))


;;(defun -get-items (feed xml-dom &key type)
;;  (with-accessors ((items items)) feed
;;    (loop for item across (get-items xml-dom type)
;;          do (push (make-item xml-dom type) items)
;;          finally (return items)))) 

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

(defun filter-feed (feed function &key key)
  (setf (items feed)
        (remove-if-not function (items feed)
                       :key key))
  feed)

(defgeneric transform (item transform)
  (:documentation "transform a feed entity by TRANSFORM: the
function will be called with either a feed or a item as an arguments
and, if called upon a feed, it'll automatically be mapped across the
feed's items after being called on the feed. We do not use the results
of this mapping directly, however any modifications to an item mutate
the original.")

  (:method :around (item transform)
           (call-next-method)
           item)

  (:method ((feed feed-entity) transform)
    (funcall transform feed))

  (:method :after ((feed feed) transform)
           (map nil (lambda (it)
                      (transform it transform))
                (items feed))))

(defun transform-content (item function)
  (setf (content item)
        (funcall function
                 (content item))))

(defun shorten-link (link)
  (let ((link (cl-ppcre:regex-replace "^https?:" link "")))
    (subseq link 0 (min 30 (length link)))))

(defmethod push-item ((feed feed) (item item))
  (push item
        (items feed)))

(deftest push-item ()
  (let ((feed (make-instance 'feed))
        (item (make-instance 'item)))
    (with-accessors ((items items)) feed
      ;;(should signal error (push-item feed 2))
      (should be eql item
              (progn
                (push-item feed item)
                (car items))))))

;; vim: set foldmethod=marker:
