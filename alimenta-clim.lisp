(defpackage :alimenta-clim
  (:use :clim-lisp :alexandria :serapeum :fw.lu))

(in-package :alimenta-clim)

(defclass feed-view (clim:view) ())
(defclass item-view (clim:view) ((%item :initarg :item :accessor item)))

(defparameter *feed-view* (make-instance 'feed-view))

(clim:define-application-frame alimenta ()
  ()
  (:pointer-documentation t)
  (:panes
    (app :application
         :height 500
         :width  500
         :display-function #'display-app
         :default-view *feed-view*)
    (int :interactor
         :height 500
         :width 500))
  (:layouts
    (default (clim:vertically () app int))  
    (flopped (clim:horizontally () app int))))

(defparameter *articles*
  (let ((errors 0))
    (handler-bind ((simple-error (lambda (c) c
                                   (incf errors)
                                   (when (< errors 1000)
                                     (invoke-restart 'alimenta.rss::pop-token)))))
      (alimenta.pull-feed:pull-feed "http://planet.lisp.org/rss20.xml" :type :rss))))

(defgeneric display-pane-with-view (frame pane view))

(defun display-app (frame pane)
  (display-pane-with-view frame pane (clim:stream-default-view pane)))

(defmethod display-pane-with-view (frame pane (view feed-view))
  (clim:with-text-style (pane (clim:make-text-style :serif :bold :larger))
    (format pane "~a <~a>~%"
            (alimenta::title *articles*)
            (alimenta::link *articles*)))
  (dolist (item (alimenta::items *articles*))
    (clim:with-output-as-presentation (pane item 'alimenta:item)
      (format pane "~a~%" (alimenta::title item)))))

(defmethod display-pane-with-view (frame pane (view item-view))
  (let ((item (item view)))
    (with-accessors ((title alimenta::title)) item
      (clim:with-output-as-presentation (pane item 'alimenta:item)
        (clim:with-text-style (pane (clim:make-text-style :serif :bold :larger))
          (format pane "~a <~a>~%"
                  (alimenta::title item)
                  (alimenta::link item)))))
    (let ((text (funcall (compose #'plump:text #'plump:parse)
                         (alimenta::content item))))
      (format pane "~&~{~{~a~^ ~}~^~%~}~2&"
              (remove-if #'null
                         (mapcar #'tokens
                                 (split-sequence #\newline
                                                 text)))))))

(define-alimenta-command (com-inspect :name t) ()
  (clouseau:inspector
    *articles*))

(define-alimenta-command (com-quite :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-alimenta-command (to-feed :name t) ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'app)))
    (setf (clim:stream-default-view pane) *feed-view*)))

(define-alimenta-command (com-pick-item :name t) ((item 'alimenta:item))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'app)))
    (setf (clim:stream-default-view pane) (make-instance 'item-view :item item))))

(define-alimenta-command (flop-layout :name t) ()
  (let ((old-view (clim:frame-current-layout clim:*application-frame*)))
    (setf (clim:frame-current-layout clim:*application-frame*)
          (case old-view
            ('default  'flopped)
            (t 'default)))))

(clim:run-frame-top-level
  (clim:make-application-frame 'alimenta-clim::alimenta))
