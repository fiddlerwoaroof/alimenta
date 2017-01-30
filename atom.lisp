(declaim (optimize (debug 3) (safety 3) (speed 0)))
(in-package :alimenta.atom)

(defclass atom-category ()
  ((term :initarg :term :initform nil :accessor term)
   (label :initarg :label :initform nil :accessor label)
   (scheme :initarg :scheme :initform nil :accessor scheme)))

(defclass atom-person ()
  ((name  :initarg :name  :type (or null string) :initform nil :accessor name)
   (uri   :initarg :uri   :type (or null string) :initform nil :accessor uri )
   (email :initarg :email :type (or null string) :initform nil :accessor email)))

(defclass atom-feed (alimenta:feed)
  ((subtitle   :initarg :subtitle                        :initform nil :accessor subtitle)
   (id         :initarg :id                              :initform nil :accessor id)
   (icon       :initarg :icon                            :initform nil :accessor icon)
   (categories :initarg :categories :type (or null list) :initform nil :accessor categories)
   (logo       :initarg :logo                            :initform nil :accessor logo)
   (updated    :initarg :updated                         :initform nil :accessor updated)
   (authors    :initarg :authors    :type (or null list) :initform nil :accessor authors)))

(defclass alimenta::link ()
  ((alimenta::relation :initarg :rel)
   (alimenta::target   :initarg :target)))

(defclass atom-item (alimenta:item)
  ((author-uri :initarg :author-uri :initform nil :accessor author-uri)))

(defun make-category (term &optional label scheme)
  (make-instance 'atom-category :term term :label label :scheme scheme))

(defun make-person (name &optional uri email)
  (make-instance 'atom-person :name name :uri uri :email email))

(defmethod alimenta::get-items (xml-dom (feed-type (eql :atom)))
  ($ (inline xml-dom) "feed > entry"))

(defun get-link (xml)
  "This only handles alternate links"
  (let ((links ($ (inline xml) "> link[rel=alternate]" (combine (attr :type) (attr :href)))))
    (map 'list
         (lambda (x)
           (destructuring-bind (type href) x
             (setf (alimenta::link (make-keyword (string-upcase type)))
                   (cons type href))))
         links)))

(defmethod make-item (xml-dom (type (eql :atom)))
  (let* ((lquery:*lquery-master-document* xml-dom)
         (item-title ($ "> title" (text) (node)))
         (links ($ "> link" (combine (attr "rel") (attr "href"))))
         (sel-links (cadr (find-if (lambda (x) (aif (car x) (equal it "alternate") t))
                                   links)))
         (item-link (or sel-links (cdr (when (> (length links) 0) (elt links 0)))))
         (item-date (or ($ "> updated" (text) (node))
                        ($ "> published" (text) (node)))) ;; Which should be default?
         (item-guid ($ "> id" (text) (node)))
         (item-description ($ "> summary" (text) (node)))
         (item-content ($ "> content" (text) (node)))
         (item-author ($ "> author > name" (text) (node)))
         (item-author-uri ($ "> author > uri" (text) (node)))
         (*tag-dispatchers* *html-tags*)
         (content (with-output-to-string (s)
                    (awhen (or item-content item-description) (serialize  (parse it) s)))))
    (make-instance 'atom-item
         :doc xml-dom
                   :content content
                   :date (local-time:parse-timestring item-date)
                   :id item-guid
                   :author item-author
                   :author-uri item-author-uri
                   :link item-link
                   :title item-title)))

(defun get-authors (xml-dom)
  (let ((authors ($ (inline xml-dom) "feed > author")))
    (loop for author across authors
          collect (make-person
                    ($ (inline author) "> name" (text) (node))
                    ($ (inline author) "> uri" (text) (node))
                    ($ (inline author) "> email" (text) (node))))))

(defmethod alimenta::-to-feed (xml-dom (type (eql :atom)) &key feed-link)
  (declare (ignore type))
  (flet ((get-feed-elem (selector) ($ (inline xml-dom) selector (text) (node)))
         (get-feed-elem-attr (selector attr) ($ (inline xml-dom) selector (attr attr) (node))))
    (let ((doc-title (get-feed-elem "feed > title"))
          (doc-subtitle (get-feed-elem "feed > subtitle"))
          (doc-summary (get-feed-elem "feed > summary"))
          (doc-icon (get-feed-elem "feed > icon"))
          (doc-logo (get-feed-elem "feed > logo"))
          (doc-id (get-feed-elem "feed > id"))
          (doc-updated (awhen (get-feed-elem "feed > updated") (local-time:parse-timestring it)))
          (doc-link (get-feed-elem-attr "feed > link[rel=alternate]" "href"))
          (doc-feed-link (or feed-link (get-feed-elem-attr "feed > link[rel=self]" "href")))
          (doc-categories ($ (inline xml-dom) "feed > category"
                             (combine (attr "term") (attr "label") (attr "scheme"))
                             (map-apply #'make-category)))
          (doc-authors (get-authors xml-dom)))
      (make-instance 'atom-feed
        :title doc-title
        :description doc-summary
        :icon doc-icon
        :logo doc-logo
        :link doc-link
        :updated doc-updated
        :id doc-id
        :feed-link doc-feed-link
        :subtitle doc-subtitle
        :categories (coerce doc-categories 'list)
        :authors doc-authors
        ))))
;}}}

(defmacro defconstants (&body constants)
  (list*
    'progn
    (loop for (name value &optional doc) in constants
        collect `(defconstant ,name ,value ,@(when doc (list doc))))))

(defvar *defconstants-really-verbose* nil)
#+sbcl (defmacro defconstants-really (&body constants)
  "auto-invoke the continue restart . . ."
  `(handler-bind ((sb-ext:defconstant-uneql
                    (lambda (c)
                      (when *defconstants-really-verbose*
                        (format t "~&Changing definition of ~s from ~s to ~s~%"
                                (sb-ext:defconstant-uneql-name c)
                                (sb-ext:defconstant-uneql-old-value c)
                                (sb-ext:defconstant-uneql-new-value c)))
                      (continue c))))
     (defconstants ,@constants)))

#-sbcl (defmacro defconstants-really (&body constants)
	 `(defconstants ,@constants))

(defmethod generate-xml ((feed feed) (feed-type (eql :atom)) &key partial)
  (let ((feed-root (or ($1 (inline partial) "feed")
                    (plump:make-element (plump:make-root) "feed"))))
    (prog1 feed-root
      (with-accessors ((title title) (id id) (updated updated) (link link)
                       (feed-link feed-link) (description description)) feed
        ($ (inline (make-element feed-root "title")) (text title)

           (inline (make-element feed-root "link"))
           (attr "href" feed-link) (attr "rel" "self")

           (inline (make-element feed-root "link"))
           (attr "href" link) (attr "rel" "alternate") (attr "type" "text/html")

           (inline (make-element feed-root "id")) (text id) (node)
           (inline (make-element feed-root "summary")) (text description) (node)
           (inline (make-element feed-root "updated")) (text updated) (node)
           )))))


(defmethod generate-xml ((item item) (feed-type (eql :atom)) &key partial)
  (let ((parent (if (string-equal (tag-name partial) "feed")
                  partial
                  (plump:make-element (plump:make-root) "feed"))))
    (prog1 parent
      (let ((item-root (make-element parent "entry")))
        (with-accessors ((title title) (id id) (date date) (link link)
                         (content content) (author author) (author-uri author-uri)) item
          ($ (inline (make-element item-root "title")) (text title)
             (inline (make-element item-root "link")) (attr "href" link)
             (inline (make-element item-root "id")) (text id) (node)
             (inline (make-element item-root "pubDate")) (text date)
             (inline (make-element item-root "author"))
             (append ($ (inline (make-element item-root "name")) (text author)))
             (append ($ (inline (make-element item-root "uri"))  (text author-uri)))
             (inline (make-element item-root "content")) (text content)
             (inline (make-element item-root "updated")) (text date) (node)
             ))))))


(defconstants-really
  (+title+ "The Title")
  (+author+ "Joe Q Public")
  (+author-uri+ "http://example.com/joeq")
  (+content+ "Teh Content")
  (+id+ "t3_43tjwv")
  (+link+ "http://example.com/something")
  (+published+ "2016-02-02T09:41:27+00:00")

  (+entry1+
    (format nil
            "<entry>
             <author>
             <name>~A</name>
             <uri>~A</uri>
             </author>
             <category term='programming' label='/r/programming'/>
             <content type='html'>~A</content>
             <id>~a</id>
             <link href='~a'/>
             <published>~a</published>
             <title>~a</title>
             </entry>"
            +author+ +author-uri+ +content+ +id+ +link+
            +published+ +title+))

    (+feed-category-term+ "testing")
    (+feed-category-label+ "/r/testing")
    (+feed-id+ "The Feed")
    (+feed-icon+ "http://example.com/feed.png")
    (+feed-logo+ "http://example.com/logo.png")
    (+feed-link-website+ "http://example.com")
    (+feed-link-self+ "http://example.com/atom.xml")
    (+feed-subtitle+ "The SubTitle")
    (+feed-title+ "The Title")
    (+feed-author-name+ "The Author")
    (+feed-author-uri+ "http://example.com/theauthor")
    (+feed-description+ "The description")
    (+feed1+
      (format nil
              "<feed>
               <title>~a</title>
               <subtitle>~a</subtitle>
               <icon>~a</icon>
               <category term=\"~a\" label=\"~a\"/>
               <link rel=\"alternate\" href=\"~a\" type=\"text/html\" />
               <link rel=\"self\" href=\"~a\" />
               <logo>~a</logo>
               <summary>~a</summary>
               <author><name>~a</name><uri>~a</uri></author>
               <author><name>~a</name><uri>~a</uri></author>
               <id>~a</id>
               </feed>"
              +feed-title+
              +feed-subtitle+
              +feed-icon+
              +feed-category-term+ +feed-category-label+
              +feed-link-website+
              +feed-link-self+
              +feed-logo+
              +feed-description+
              +feed-author-name+ +feed-author-uri+
              +feed-author-name+ +feed-author-uri+
              +feed-id+
              )))

(defun true (x) (not (null x)))

(defun get-node-text (xml-doc selector)
  ($ (inline xml-doc) selector (text) (node)))

(deftest to-feed ()
  (let ((xml (parse +feed1+)))
    (symbol-macrolet ((feed (alimenta::-to-feed xml :atom)))
      (should be equal +feed-title+ (slot-value feed 'alimenta:title))
      (should be equal +feed-link-website+ (slot-value feed 'alimenta:link))
      (should be equal +feed-link-self+ (slot-value feed 'alimenta:feed-link))
      (should be equal +feed-description+ (slot-value feed 'description))
      (should be equal +feed-id+ (slot-value feed 'id))
      (should be equal +feed-subtitle+ (slot-value feed 'subtitle))
      (should be equal +feed-icon+ (slot-value feed 'icon))
      (should be equal +feed-logo+ (slot-value feed 'logo))

      (should be equal +feed-category-term+
              (slot-value
                (elt
                  (slot-value feed 'categories)
                  0)
                'term))
      (should be equal +feed-category-label+
              (slot-value
                (elt
                  (slot-value feed 'categories)
                  0)
                'label))

      (should be equal +feed-author-name+
              (slot-value
                (elt (slot-value feed 'authors) 0)
                'name))
      (should be equal +feed-author-uri+
              (slot-value
                (elt (slot-value feed 'authors) 0)
                'uri))

      ;(should be equal +feed-title+ (slot-value feed 'alimenta:title))
      ;(should be equal +feed-title+ (slot-value feed 'alimenta:title))
      ;(should be equal +feed-title+ (slot-value feed 'alimenta:title))
      )
    )
  )

(deftest make-item ()
  (let ((xml (lquery:$ (inline (plump:parse +entry1+)) "entry" (node))))
    (symbol-macrolet ((item (alimenta::make-item xml :atom)))
      (should be true item)
      (should be equal +link+ (slot-value item 'alimenta:link))
      (should be equal +content+ (slot-value item 'alimenta:content))
      (should be equal +author+ (slot-value item 'alimenta::author))
      (should be equal +author-uri+ (slot-value item 'author-uri))
      (should be equal +id+ (slot-value item 'alimenta:id)))))

(defparameter *tmp* nil)
(deftest generate-xml ()
  (let* ((xml ($ (inline (parse +entry1+)) "entry" (node)))
         (item (alimenta::make-item xml :atom)))
    (symbol-macrolet ((generated-xml (alimenta::generate-xml item :atom)))
      (should be equal +title+
              ($ (inline generated-xml) "entry > title" (text) (node)))
      (should be equal +author+
              ($ (inline generated-xml) "entry > author > name" (text) (node)))
      (should be equal +author-uri+
              ($ (inline generated-xml) "entry > author > uri" (text) (node)))
      (should be equal +id+
              ($ (inline generated-xml) "entry > id" (text) (node)))
      (should be equal +content+
              ($ (inline generated-xml) "entry > content" (text) (node)))
      (should be equal +link+
              ($ (inline generated-xml) "entry > link" (attr "href") (node)))
      ; TODO: deal with dates . . .
      )))

(defun do-test (&optional (test nil))
  (let ((st:*test-output* *debug-io*))
    (multiple-value-bind (result hm? errors) (st:test :test test)
      (format t
              "~&Returns: ~a~%Error:~%~{~a~^~%~}~%Failures-vals:~%~{~a~^ ~}~%"
              result
              errors
              hm?
              ))))
