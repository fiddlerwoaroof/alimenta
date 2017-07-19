(defpackage :alimenta-clim
  (:use :clim-lisp :alexandria :serapeum :fw.lu))

(in-package :alimenta-clim)

(defclass feed-list (clim:view)
  ((%feeds :initarg :feeds :initform '() :accessor feeds)))

(defmethod initialize-instance :after ((object feed-list) &key feeds)
  (setf (feeds object) (copy-seq feeds)))

(defclass feed-view (clim:view) ())
(defclass item-view (clim:view) ((%item :initarg :item :accessor item)))

(defclass feed-url ()
  ((%uri :initarg :uri :reader uri)))

(defmethod displayed-feeds ((feed-list feed-list))
  (feeds feed-list))

(defparameter *feed-list* (make-instance 'feed-list))
(defparameter *feed-view* (make-instance 'feed-view))

(clim:define-application-frame alimenta ()
  ((%feed-list :initarg :feed-list :initform (make-instance 'feed-list) :reader feed-list))
  (:menu-bar t)
  (:pointer-documentation t)
  (:panes
   (feeds :application
          :height 400
          ;; :width 100
          :display-function 'display-app
          :default-view (clim:with-application-frame (frame)
                          (feed-list frame)))
   (items :application
          :height 400
          ;; :width  200
          :display-function #'display-app
          :default-view *feed-view*)
   (articles :application
             :height 400
             ;; :width 300
             :display-function 'display-app
             )
   (int :interactor
        :height 200
        :width 600))
  (:layouts
   (default (clim:vertically () (clim:horizontally ()
                                  feeds
                                  items
                                  articles)
                             int))  
   (flopped (clim:horizontally () feeds items articles int))))

(defparameter *feeds* '("https://sancrucensis.com/feed/"
                        "https://thomism.wordpress.com/feed/"))

(defparameter *articles*
  (let ((errors 0))
    (handler-bind ((simple-error (lambda (c) c
                                         (incf errors)
                                         (when (< errors 1000)
                                           (invoke-restart 'alimenta.rss::pop-token)))))
      (alimenta.pull-feed:pull-feed "http://planet.lisp.org/rss20.xml" :type :rss))))

(defgeneric display-pane-with-view (frame pane view))

(defmethod display-pane-with-view (frame pane view)
  (format pane "~&No content~%"))

(defun display-app (frame pane)
  (display-pane-with-view frame pane (clim:stream-default-view pane)))

(defmethod display-pane-with-view (frame pane (view feed-list))
  (clim:with-text-style (pane (clim:make-text-style :serif :bold :larger))
    (format pane "Feeds~%"))
  (dolist (feed (displayed-feeds view))
    (clim:with-output-as-presentation (pane feed 'feed-url)
      (format pane "~a~%" (uri feed)))))

(defmethod display-pane-with-view (frame pane (view feed-view))
  (clim:with-text-style (pane (clim:make-text-style :serif :bold :larger))
    (format pane "~a <~a>~%"
            (alimenta::title *articles*)
            (alimenta::link *articles*)))
  (dolist (item (alimenta::items *articles*))
    (clim:with-output-as-presentation (pane item 'alimenta:item)
      (format pane "~a~%" (alimenta::title item)))))

(defun format-content (node)
  (lquery:$ (initialize (alimenta:content node)) "> p" (text)
            (filter (op (string/= "" _)))
            (map (op (trim-whitespace _)))))

(defmethod display-pane-with-view (frame pane (view item-view))
  ;; (format *xxx* "~&Displaying view~%")
  (let ((item (item view)))
    (with-accessors ((title alimenta::title)) item
      (clim:with-output-as-presentation (pane item 'alimenta:item)
        (clim:with-text-style (pane (clim:make-text-style :serif :bold :larger))
          (format pane "~a <~a>~%"
                  (alimenta::title item)
                  (alimenta::link item)))))
    (let ((text (plump:text (plump:parse (alimenta::content item)))))
      (format pane "~&~{~{~a~^ ~}~^~%~}~2&"
              (remove-if #'null
                         (mapcar #'tokens
                                 (split-sequence #\newline
                                                 text)))))))

;; (define-alimenta-command (com-inspect :name t) ()
;;   (clouseau:inspector
;;    *articles*))

(define-alimenta-command (com-quit :name t :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(defmacro with-interactor ((interactor-symbol) &body body)
  `(let ((,interactor-symbol (clim:find-pane-named clim:*application-frame* 'int)))
     ,@body))

(define-alimenta-command (add-feed :name t) ((url string))
  (let ((int (clim:find-pane-named clim:*application-frame* 'int)))
    (push (make-instance 'feed-url :uri url)
          (feeds (feed-list clim:*application-frame*)))
    (format int "~&Added feed ~A~%" url)))

(define-alimenta-command (open-feed :name t) ((url feed-url :gesture :select))
  (let ((int (clim:find-pane-named clim:*application-frame* 'int))
        (app (clim:find-pane-named clim:*application-frame* 'items)))
    (setf *articles* (alimenta.pull-feed:pull-feed (uri url)))
    (format int "~&Switching to the feed view~%")
    (setf (clim:stream-default-view app) *feed-view*)))

(define-alimenta-command (to-feed :name t) ()
  (let ((int (clim:find-pane-named clim:*application-frame* 'int))
        (app (clim:find-pane-named clim:*application-frame* 'items)))
    (format int "~&Switching to the feed view~%")
    (setf (clim:stream-default-view app) *feed-view*)))

(define-alimenta-command (com-pick-item :name t) ((item 'alimenta:item :gesture :select))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'articles)))
    (format pane "~&ARTICLE PANE~%")
    (with-interactor (int)
      (format int "~&Switching to the item: ")
      (clim:with-output-as-presentation (int item 'alimenta:item)
        (format int "~a" (alimenta:title item)))
      (terpri int))
    (setf (clim:stream-default-view pane)
          (make-instance 'item-view :item item))))

(define-alimenta-command (flop-layout :name t) ()
  (let ((old-view (clim:frame-current-layout clim:*application-frame*)))
    (setf (clim:frame-current-layout clim:*application-frame*)
          (case old-view
            ('default  'flopped)
            (t 'default)))))


(defun main ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'alimenta-clim::alimenta
                                :feed-list
                                (make-instance 'feed-list
                                               :feeds (list (make-instance 'feed-url
                                                                           :uri
                                                                           "http://thomism.wordpress.com/feed")
                                                            (make-instance 'feed-url
                                                                           :uri
                                                                           "http://planet.lisp.org/rss20.xml"))))))
