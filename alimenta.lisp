;;;; alimenta.lisp
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:alimenta)

(defclass feed () 
  ((title :initarg :title :initform nil)
   (link :initarg :link :initform nil)
   (items :initarg :items :initform nil)
   (description :initarg :description :initform nil)
   (feed-link :initarg :feed-link :initform nil)
   (doc :initarg :doc :initform nil)
   (source-type :initarg :source-type :initform nil)))

(defclass item ()
  ((title :initarg :title :initform nil)
   (id :initarg :id :initform nil)
   (author :initarg :author :initform nil)
   (date :initarg :date :initform nil)
   (link :initarg :link :initform nil)
   (links :initform (make-hash-table :test #'equalp))
   (content :initarg :content :initform nil)
   (doc :initarg :doc :initform nil)))

(defclass complex-value () ())

(defgeneric primary-value (self)
  (:documentation "Primarily for COMPLEX-VALUES: this should take one and return a useful primary value")
  (:method ((self t)) self))

(define-condition duplicate-link-type (error)
  ((old :reader duplicate-link-type-old :initarg :old)
   (new :reader duplicate-link-type-new :initarg :new))
  (:report (lambda (condition stream)
             (format stream "Item already has link ~s" (duplicate-link-type-old condition)))))


(defgeneric push-item (feed item)
  (:documentation "Adds an item to the feed"))

(defgeneric make-item (xml-dom doc-type)
  (:documentation "Given an xml document, return an item"))

(defgeneric parse-feed (feed))

(defgeneric %get-items (xml feed-type))

(defgeneric %generate-xml (feed feed-type &key partial))
(defmethod %generate-xml :around ((feed feed) feed-type &rest r)
  (declare (ignore r))
  (let ((result (call-next-method feed feed-type)))
    (with-slots (items) feed
      (loop for item in items
            do (%generate-xml item feed-type :partial result)))
    result))

(defgeneric %to-feed (doc type &key feed-link)
  (:documentation "Given an xml-document, return a feed object"))
(defmethod %to-feed :around ((xml-dom plump:node) doc-type &key feed-link)
  "This wraps the particular methods so that _they_ don't have to implement item fetching.
   NIL passed to the type activates auto-detection"
  (aprog1 (call-next-method xml-dom doc-type :feed-link feed-link)
    (with-slots (doc source-type) it
      (setf doc xml-dom
            source-type doc-type))
    (with-slots (items) it
      (setf
        items (loop for item across (%get-items xml-dom doc-type)
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


(defun generate-xml (feed &key (feed-type :rss))
  (%generate-xml feed feed-type))

(defun to-feed (doc &key type feed-link)
  "Makes an instance of feed from the given document.  Specialize %to-feed with
   an equal-specializer on type with an appropriate symbol to implement a new
   sort of feed."
  (unless type
    (setf type (detect-feed-type doc)))
  (%to-feed doc type :feed-link feed-link))


(defun get-items (feed xml-dom &key type)
  (with-slots (items) feed
    (loop for item across (%get-items xml-dom type)
          do (push (make-item xml-dom type) items)
          finally (return items)))) 

(defgeneric feed-to-rss (feed))
(defgeneric feed-to-atom (feed))
(defgeneric feed-to-json (feed))
(defgeneric feed-to-html5 (feed)
  (:documentation
    "take a feed object, produce an html5 output.  Simple format:
     <!DOCTYPE html>
     <html lang=\"en\">
     <head>
       <meta charset=\"UTF-8\">
       <title>Feed Title</title>
     </head>
     <body>
       <main>
         <article id=\"id\">
           <h1>Title</h1>
           <h2>Author</h2>
           <span class=\"date\">Date</span>
           <p>Content</p>
         </article>
       </main>
     </body>
     </html>"))

(defun make-feed (&key title link items feed-link description)
  (make-instance 'feed :title title :link link :items items :feed-link feed-link :description description))

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

(defun rdf-to-feed (xml-dom))
(defun json-to-feed (json-object))
(defun html5-to-feed (html-dom))

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
