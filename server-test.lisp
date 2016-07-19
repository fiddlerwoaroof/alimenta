(in-package :cl-user)
(ql:quickload :clack)
(ql:quickload :ningle )
(ql:quickload :araneus)
(ql:quickload :spinneret)
(ql:quickload :lass)

(defun get-css ()
  (lass:compile-and-write
    `(*
       :padding "0px"
       :margin "0px")

    `(body
       :box-sizing "border-box"
       :font-family sans-serif
       :background "#888")

    `(header
       :border-bottom "thin solid black"
       :width "100%"
       :text-align center
       :margin-bottom "2em"
       :padding "1em")

    `(div.articles
       :display "block"
       :flex-flow "row"
       :flex-wrap "wrap-reverse"
       :align-items "baseline"
       :justify-content "space-around"
       :align-content "space-between"

       )

    `(article
       :padding "1em"
       :border "4px double #888"
       :vertical-align "middle"
       :width "100%"
       :overflow "hidden"
       :min-height "4em"
       :background "#aaa"

       (div.title
         :float "left"
         :width "50%")

       (a.link
         :float "left"
         :clear "left"
         :width "50%")

       (div.content
         :margin-top "-3em"
         :padding-right "5em"
         :float "right"
         :width "50%")

       )
    ))

(defmethod araneus:view ((name (eql 'root)) (item alimenta:item))
  (with-slots ((title alimenta:title) (link alimenta:link) (content alimenta:content)) item
    (spinneret:with-html 
      (:article
        (:div.title title)
        (:a.link :href link link)
        (:div.content (:raw content))))))

(defmethod araneus:view ((name (eql 'root)) (feed alimenta:feed))
  (with-slots ((title alimenta:title) (link alimenta:link)) feed
    (spinneret:with-html
      (:header
        (:h1.feed-title title)
        (:a.feed-link link)))))

(defmethod araneus:view :around ((name (eql 'root)) (feed alimenta:feed))
  (with-slots ((title alimenta:title) (items alimenta::items)) feed
    (spinneret:with-html-string
      (:html
        (:head (:title title))
        (:style
          :type "text/css"
          (get-css))
        (:body
          (:main
            (call-next-method)
            (:div.articles
              (loop for item in items
                    do (araneus:view 'root item)))))))))

(araneus:define-controller root (params)
  (declare (optimize (debug 3)))
  (let* ((url "http://reddit.com/r/prolog.rss")
         (feed (alimenta.pull-feed::fetch-doc-from-url url)))
    (alimenta:to-feed feed :feed-link url)))

(araneus:define-view feed-to-atom (feed)
  `(200
    (:content-type "application/xml+atom")
    (,(concatenate 'string
               "<?xml version=\"1.0\"?>"
               (plump:serialize (alimenta:generate-xml feed :atom)
                   nil)))))

(araneus:define-view feed-to-rss (feed)
  `(200
    (:content-type "application/xml+rss")
    (,(concatenate 'string
               "<?xml version=\"1.0\"?>"
               (plump:serialize (alimenta:generate-xml feed :rss)
                   nil)))))

(defparameter *app* (make-instance 'ningle:<app>))

(araneus:defroutes *app*
  (("/") (araneus:as-route 'root))
  (("/.rss") (araneus::compose-route (root) feed-to-rss)) 
  (("/.atom") (araneus::compose-route (root) feed-to-atom)))

(defparameter *handler* (clack:clackup *app* :port 9091 ))
