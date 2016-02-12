(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :alimenta.rss)

(defclass rss-feed (feed)
  ((language :initarg :language :initform nil)
   (copyright :initarg :copyright :initform nil)
   (managing-editor :initarg :managing-editor :initform nil)
   (webmaster :initarg :webmaster :initform nil)
   (publication-date :initarg :publication-date :initform nil)
   (last-build-date :initarg :last-build-date :initform nil)
   (categories :initarg :categories :initform nil)
   (generator :initarg :generator :initform nil)
   (docs :initarg :docs :initform nil)
   (cloud :initarg :cloud :initform nil)
   (ttl :initarg :ttl :initform nil)
   (image :initarg :image :initform nil)
   (rating :initarg :rating :initform nil)
   (text-input :initarg :text-input :initform nil)
   (skip-hours :initarg :skip-hours :initform nil)
   (skip-days :initarg :skip-days :initform nil)))

(defclass rss-item (feed) ())

(defmethod %get-items (xml-dom (feed-type (eql :rss)))
  ($ (inline xml-dom) "channel > item"))

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

(defmethod %generate-xml ((feed feed) (feed-type (eql :rss)) &rest r)
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

(defmethod make-item (xml-dom (type (eql :rss)))
  (let* ((item-title ($ "item > title" (text) (node)))
         (item-link ($ "item > link" (text) (node)))
         (item-date (get-date ($ "item > pubDate" (text) (node))))
         (item-guid ($ "item > guid" (text) (node)))
         (item-description ($ "item > description" (text) (node)))
         (item-content-encoded ($ "item > content::encoded" (text) (node)))
         (content (aif (or item-content-encoded item-description)
                    (with-output-to-string (s)
                      (serialize (parse (or item-content-encoded item-description)) s))))
         (*tag-dispatchers* *html-tags*))
    (make-instance 'item
                   :content content   
                   :date item-date
                   :doc xml-dom
                   :id item-guid
                   :link item-link
                   :title item-title)))

(defun get-date (str)
  (handler-case
    (local-time:parse-timestring str)
    (local-time::invalid-timestring (c) (declare (ignore c))
      (multiple-value-bind (res groups) (cl-ppcre:scan-to-strings "(.*)\s*([+-][0-9]{2,4})\s?$" str)
        (let ((local-time:*default-timezone* local-time:+utc-zone+))
          (let ((timestamp (string-trim " " (if res (elt groups 0) str)))
                (offset (if res (parse-integer (elt groups 1)) 0)))
            (local-time:timestamp- (chronicity:parse timestamp) offset :hour)))))))

(defmethod %to-feed (xml-dom (type (eql :rss)) &key feed-link)
  ; TODO: store feed-link
  (lquery:initialize xml-dom)
  (flet ((get-channel-element (el)
           ($ (inline xml-dom) el (text) (node))))
    (let ((doc-title (get-channel-element "channel > title"))
          (doc-link (get-channel-element "channel > link"))

          (doc-language (get-channel-element "channel > language"))
          (doc-copyright (get-channel-element "channel > copyright"))
          (doc-managing-editor (get-channel-element "channel > managingEditor"))
          (doc-webmaster (get-channel-element "channel > webMaster"))
          (doc-publication-date (awhen (get-channel-element "channel > pubDate") (get-date it)))
          (doc-last-build-date (awhen (get-channel-element "channel > lastBuildDate") (get-date it)))
          (doc-categories (get-channel-element "channel > category"))
          (doc-generator (get-channel-element "channel > generator"))
          (doc-docs (get-channel-element "channel > docs"))
          (doc-cloud (get-channel-element "channel > cloud"))
          (doc-ttl (get-channel-element "channel > ttl"))
          (doc-image (get-channel-element "channel > image"))
          (doc-rating (get-channel-element "channel > rating"))
          (doc-text-input (get-channel-element "channel > textInput"))
          (doc-skip-hours (get-channel-element "channel > skipHours"))
          (doc-skip-days (get-channel-element "channel > skipDays"))

          (doc-feed-link (or feed-link
                             ($ "feed > atom::link[rel=self]" (attr "href") (node)))))
      (make-instance 'rss-feed
        :title doc-title 
        :link doc-link 
        :feed-link doc-feed-link
        
        :language doc-language
        :copyright doc-copyright
        :managing-editor doc-managing-editor
        :webmaster doc-webmaster
        :publication-date doc-publication-date
        :last-build-date doc-last-build-date
        :categories doc-categories
        :generator doc-generator
        :docs doc-docs
        :cloud doc-cloud
        :ttl doc-ttl
        :image doc-image
        :rating doc-rating
        :text-input doc-text-input
        :skip-hours doc-skip-hours
        :skip-days doc-skip-days))))

