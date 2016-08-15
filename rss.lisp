(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :alimenta.rss)


(defclass rss-image ()
  ((url :initarg :url :initform nil)
   (title :initarg :title :initform nil)
   (link :initarg :link :initform nil)
   (width :initarg :width :initform nil)
   (height :initarg :height :initform nil)
   (description :initarg :description :initform nil)))

(defclass rss-category ()
  ((category :initarg :category :initform nil)
   (domain :initarg :domain :initform nil)))

(define-data-class rss-feed (doc "channel") (feed)
  language copyright webmaster
  generator docs cloud ttl rating
  (image "image"
         :value (apply #'make-image
                       (mapcar (lambda (x) (when (> (length x) 0)
                                             (plump:text (elt x 0))))
                               (let ((plump:*tag-dispatchers* plump:*xml-tags*))
                                 ($1 (inline doc) "channel > image"
                                     (combine "url" "title" "link" "width" "height"
                                              "description")))))) 

  (categories "category" :value (get-categories doc  "channel > category"))
  (text-input "textInput")
  (managing-editor "managingEditor") 
  (skip-days "skipDays")
  (skip-hours "skipHours") 
  (publication-date "publicationDate" :transform get-date) 
  (last-build-date "lastBuildDate" :transform get-date))

(define-data-class rss-item (doc "") (item)
  (categories "category" :value (get-categories doc "> category"))
  source comments enclosure )

(defmethod print-object ((self rss-image) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (slot-value self 'url))))

(defmethod print-object ((self rss-category) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a~@[ ~a~]"
            (slot-value self 'category)
            (slot-value self 'domain))))

(defmacro check ((test &body body))
  `(let ((val (progn ,@body)))
     (when (,test val)
       val)))

(defun all-alpha (str)
  (check-type str string)
  (loop for char across str
        always (alpha-char-p char)))


(defun extract-date-timezone (date-str)
  (declare (optimize (debug 3)))
  (let ((tz-inited nil))
    (flet ((init-tz ()
             (unless tz-inited
               (local-time:reread-timezone-repository)
               (setf tz-inited t))))

      (macrolet ((ensure-tz-inited (&body body)
                   `(progn (init-tz)
                           ,@body)))
        (let* ((last-space (position #\space date-str :from-end t))
               (tz-name (check (all-alpha (subseq date-str (1+ last-space)))))
               (timestamp-raw (if tz-name
                                (subseq date-str 0 last-space)
                                date-str)))
          (values (if tz-name
                    (ensure-tz-inited
                      (local-time:find-timezone-by-location-name
                        (string-upcase tz-name)))
                    local-time:+utc-zone+)
                  timestamp-raw))))))

(defun get-date (str)
  (declare (optimize (debug 3)))
  (handler-case
    (local-time:parse-timestring str)
    (local-time::invalid-timestring (c) (declare (ignore c))
      (multiple-value-bind (local-time:*default-timezone* timestamp-raw) (extract-date-timezone str)
        (multiple-value-bind (res groups) (cl-ppcre:scan-to-strings "(.*)\s*([+-][0-9]{2,4})\s?$" timestamp-raw)
          (let ((ts (if res (elt groups 0) timestamp-raw))
                (tz-offset (if res (elt groups 1) "0000")))
            (let* ((timestamp (string-trim " " ts))
                   ; Handle numeric timzones like -0430 or +0320
                   (hour-offset (parse-integer tz-offset :end 3))
                   (minute-offset (if (> (length tz-offset) 3)
                                    (* (signum hour-offset)
                                       (parse-integer tz-offset :start 3))
                                    0)))

              (loop
                (restart-case (return
                                (let-each (:be *)
                                  (chronicity:parse timestamp)
                                  (local-time:timestamp- * minute-offset :minute)
                                  (local-time:timestamp- * hour-offset   :hour)))       
                  (pop-token () (setf timestamp
                                      (subseq timestamp
                                              0
                                              (position #\space timestamp
                                                        :from-end t)))))))))))))

(defun pop-token ()
  (when-let ((restart (find-restart 'pop-token)))
    (invoke-restart restart)))

(defmethod primary-value ((self rss-image))
  (slot-value self 'url))

(defun make-image (url title &optional link width height description)
  (let ((link (or link url)))
    (make-instance 'rss-image
                   :url url
                   :title title
                   :link link
                   :width width
                   :height height
                   :description description)))

(defmethod primary-value ((self rss-category))
  (slot-value self 'category))

(defun make-category (category &optional domain)
 (make-instance 'rss-category :category category :domain domain))

(defun get-categories (doc tag)
  ($ (inline doc) tag
     (combine (text) (attr "domain"))
     (map-apply #'make-category)))

(defmethod alimenta::get-items (xml-dom (feed-type (eql :rss)))
  ($ (inline xml-dom) "channel > item"))

(defmethod generate-xml ((item item) (feed-type (eql :rss)) &key partial)
  (prog1 partial
    (let ((item-root (make-element ($1 (inline partial) "channel") "item")))
      (flet ((make-element (tag) (make-element item-root tag)))
        (with-slots (title id date link content) item
          ($ (inline (make-element "title")) (text title)
            (inline (make-element "link")) (text link)
            (inline (make-element "pubDate")) (text date)
            (inline (make-element "description")) (text content))    
          (plump-dom:set-attribute
            ($ (inline (make-element "guid")) (text id) (node))
            "isPermaLink"
            "false"))))))

(defmethod generate-xml ((feed feed) (feed-type (eql :rss)) &rest r)
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
  (let* ((*lquery-master-document* xml-dom)
         (item-title ($1 "> title" (text)))
         (item-link ($1 "> link" (text)))
         (item-date (awhen ($1 "> pubDate" (text)) (get-date it)))
         (item-guid ($1 "> guid" (text)))
         (item-description ($1 "> description" (text)))
         (item-content-encoded ($1 "> content::encoded" (text)))
         (content (aif (or item-content-encoded item-description)
                    (with-output-to-string (s)
                      (serialize (parse (or item-content-encoded item-description)) s))))
         (*tag-dispatchers* *html-tags*))
    (make-instance 'rss-item
                   :content content   
                   :date item-date
                   :doc xml-dom
                   :id item-guid
                   :link item-link
                   :title item-title)))

(deftest get-date ()
  (should be local-time:timestamp=
          (local-time:parse-timestring "2016-01-09T23:00:00.000000-0100")
          (get-date "Fri, 09 Jan 2016 23:00:00-0100"))
  (should be local-time:timestamp=
          (local-time:parse-timestring "2016-01-09T23:00:00.000000-0100")
          (get-date "Fri, 09 Jan 2016 23:00:00 -0100"))
  (should be local-time:timestamp=
          (local-time:parse-timestring "2016-01-09T23:00:00.000000-0100")
          (get-date "Fri, 09 Jan 2016 22:00:00 -0200"))  
  (should be local-time:timestamp=
          (local-time:parse-timestring "2016-01-09T23:00:00.000000-0100")
          (get-date "Fri, 09 Jan 2016 21:30:00 -0230"))) 

(defmethod alimenta::-to-feed (xml-dom (type (eql :rss)) &key feed-link)
  ; TODO: store feed-link
  (flet ((get-channel-element (el)
           ($ (inline xml-dom) el (text) (node))))
    (let* ((*lquery-master-document* xml-dom)
           (doc-title (get-channel-element "channel > title"))
           (doc-link (get-channel-element "channel > link"))
           (doc-description (get-channel-element "channel > description"))
           (doc-feed-link (or feed-link
                              ($ "channel > atom::link[rel=self]" (attr "href") (node)))))
      (make-instance 'rss-feed
        :title doc-title
        :link doc-link
        :description doc-description
        :feed-link doc-feed-link))))

