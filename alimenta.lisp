;;;; alimenta.lisp
(declaim (optimize (speed 0) (safety 3) (debug 3)))


(in-package #:alimenta)

;;; "alimenta" goes here. Hacks and glory await!
(defun detect-feed-type (xml-dom)
  (let ((root-node-name (make-keyword (string-upcase
                                        ($ (inline xml-dom) (children)
                                           (map #'tag-name) (node))))))
    (setf type
          (case root-node-name
            ((:feed) :atom)
            (t root-node-name)))))


(defgeneric push-item (feed item)
  (:documentation "Adds an item to the feed"))

(defgeneric make-item (xml-dom type))

(defgeneric parse-feed (feed))

(defgeneric %to-feed (doc type &key feed-link))

(defgeneric %generate-xml (feed feed-type &key partial))

(defun generate-xml (feed &key (feed-type :rss))
  (%generate-xml feed feed-type))

(defun to-feed (doc &key type feed-link)
  "Makes an instance of feed from the given document.  Specialize %to-feed with
   an equal-specializer on type with an appropriate symbol to implement a new
   sort of feed."
  (unless type
    (setf type (detect-feed-type doc)))
  (%to-feed doc type :feed-link feed-link))

(defgeneric %get-items (xml feed-type)
  (:method (xml-dom (feed-type (eql :rss))) ($ (inline xml-dom) "channel > item")))

(defun get-items (feed xml-dom &key type)
  (with-slots (items) feed
    (loop for item across (%get-items xml-dom type)
          do (push (make-item xml-dom type) items)
          finally (return items)))) 

(defmethod %to-feed :around (xml-dom doc-type &key feed-link)
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

(defclass feed ()
  ((title :initarg :title :initform nil)
   (link :initarg :link :initform nil)
   (items :initarg :items :initform nil)
   (description :initarg :description :initform nil)
   (feed-link :initarg :feed-link :initform nil)
   (doc :initarg :doc :initform nil)
   (source-type :initarg :source-type :initform nil)))

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

(defmethod %generate-xml :around ((feed feed) feed-type &rest r)
  (declare (ignore r))
  (let ((result (call-next-method feed feed-type)))
    (with-slots (items) feed
      (loop for item in items
            do (%generate-xml item feed-type :partial result)))
    result))

(defmethod shorten-link (link)
  (let ((link (cl-ppcre:regex-replace "^https?:" link "")))
    (subseq link 0 (min 30 (length link)))))

(defmethod print-object ((object feed) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title link) object
      (format stream "title: ~s link: ~s"
              (aif title (shorten-link it) "<untitled>")
              (aif link (shorten-link it) "<no link>")))))

(defclass rss-feed (feed) ())

(defmethod %generate-xml ((feed rss-feed) (feed-type (eql :rss)) &rest r)
  (declare (ignore r))
  (let* ((xml-root (plump:make-root))
         (feed-root (plump:make-element xml-root "rss"))
         (channel (plump-dom:make-element feed-root "channel")))
    ($ (inline feed-root)
       (attr "version" "2.0")
       (attr "xmlns:content" "http://purl.org/rss/1.0/modules/content/")
       (attr "xmlns:atom" "http://www.w3.org/2005/Atom"))
    (with-slots (title link feed-link description) feed
      ($ (inline (make-element channel "title"))
         (text title))
      ($ (inline (make-element channel "link"))
         (text link))
      (awhen description
        ($ (inline (make-element channel "description"))
         (text it)))
      ($ (inline (make-element channel "atom:link"))
         (attr "rel" "self")
         (attr "type" "application/rss+xml")
         (attr "href" link)))
    xml-root))

(defclass item ()
  ((title :initarg :title :initform nil)
   (id :initarg :id :initform nil)
   (author :initarg :author :initform nil)
   (date :initarg :date :initform nil)
   (link :initarg :link :initform nil)
   (links :initform (make-hash-table :test #'equalp))
   (content :initarg :content :initform nil)
   (doc :initarg :doc :initform nil)))

(defgeneric (setf link) (value self))

(define-condition duplicate-link-type (error)
  ((old :reader duplicate-link-type-old :initarg :old)
   (new :reader duplicate-link-type-new :initarg :new))
  (:report (lambda (condition stream)
             (format stream "Item already has link ~s" (duplicate-link-type-old condition)))))

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

(defmethod %generate-xml ((item item) (feed-type (eql :rss)) &key partial)
  (prog1 partial
    (let ((item-root (make-element ($ (inline partial) "channel" (node)) "item")))
      (with-slots (title id date link content) item
        ($ (inline (make-element item-root "title")) (text title)) 
        ($ (inline (make-element item-root "link")) (text link)) 
        (plump-dom:set-attribute
          ($ (inline (make-element item-root "guid")) (text id) (node))
          "isPermaLink"
          "false") 
        ($ (inline (make-element item-root "pubDate")) (text date)) 
        ($ (inline (make-element item-root "description")) (text content))))))

(defmethod print-object ((object item) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title link date) object
      (format stream "title: ~s link: ~s date:~s"
              (aif title (shorten-link it) "<untitled>")
              (aif link (shorten-link it) "<no link>")
              (aif date (shorten-link it) "<no date>")))))

;{{{ RSS feed handling
(defmethod make-item (xml-dom (type (eql :rss)))
  (let* ((item-title ($ "> title" (text) (node)))
         (item-link ($ "> link" (text) (node)))
         (item-date ($ "> pubDate" (text) (node)))
         (item-guid ($ "> guid" (text) (node)))
         (item-description ($ "> description" (text) (node)))
         (item-content-encoded ($ "> content::encoded" (text) (node)))
         (content (with-output-to-string (s)
                    (serialize (parse (or item-content-encoded item-description)) s)))
         (*tag-dispatchers* *html-tags*))
    (make-instance 'item
                   :content content   
                   :date item-date
                   :doc xml-dom
                   :id item-guid
                   :link item-link
                   :title item-title)))

(defmethod %to-feed (xml-dom (type (eql :rss)) &key feed-link)
  ; TODO: store feed-link
  (lquery:initialize xml-dom)
  (let ((doc-title ($ "channel > title" (text) (node)))
        (doc-link ($ "channel > link" (text) (node)))
        (doc-feed-link (or feed-link
                           ($ "feed > atom::link[rel=self]" (first) (attr "href") (node)))))
    (make-instance 'rss-feed :title doc-title :link doc-link :feed-link doc-feed-link)))
;}}} 

; {{{ ATOM feed handling


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
