(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :alimenta.rss)

(defclass rss-feed (feed) ())
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

