(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :alimenta.pull-feed)

(defmacro setup-libraries-for-feeds (&body body)
  `(let ((plump:*tag-dispatchers* plump:*xml-tags*)
         (drakma:*drakma-default-external-format* :utf-8)
         (drakma:*text-content-types*
           (pairlis '("application" "application" "application" "application")
                    '("atom+xml"    "rss+xml"     "xml"         "rdf+xml")
                    drakma:*text-content-types*)))
     ,@body))

(defvar *user-agent* "alimenta/0.0")

(defun call-with-user-agent (user-agent cb &rest args)
  (let ((*user-agent* user-agent))
    (apply cb args)))

(defun let-bind-special-var-macro-body (var value body)
  `(let ((,var ,value))
     ,@body))

(defmacro with-user-agent ((user-agent) &body body)
  (let-bind-special-var-macro-body '*user-agent* user-agent body))

(defun fetch-doc-from-url (url)
  (setup-libraries-for-feeds
    (let ((data (drakma:http-request url :user-agent *user-agent* :decode-content t)))
      (when (and (not (stringp data))
                 (vectorp data))
        (setf data (babel:octets-to-string data)))
      (plump:parse data))))

(define-condition fetch-error (error) ())
(define-condition feed-ambiguous (fetch-error) ((choices :initarg :choices :initform nil)))
(define-condition no-feed (fetch-error) ((url :initarg :url :initform nil)))

(defun skip-feed (&optional condition)
  (when-let ((restart (find-restart 'skip-feed condition)))
    (invoke-restart restart)))

(defun feed-ambiguous (choices)
  (error 'feed-ambiguous
         :choices choices))

(defun no-feed (url)
  (cerror "Skip this feed" 'no-feed :url url))

(defun fetch-feed-from-url (url &key type)
  (setup-libraries-for-feeds
    (let* ((feeds (alimenta.discover:discover-feed
                   (drakma:http-request url
                                        :user-agent *user-agent*
                                        :decode-content t)))
           (feeds (if type (remove-if-not (lambda (x) (eql type (car x)))
                                          feeds)
                      feeds)))
      (if (not feeds)
          (no-feed url)
          (fetch-doc-from-url
           (cdar 
            (restart-case
                (if (cdr feeds) (feed-ambiguous feeds) feeds)
              (take-first-feed nil
                :report (lambda (s) (format s "Take the first feed"))
                feeds)
              (take-nth-feed (n)
                :report (lambda (s) (format s "Take the nth feed"))
                (list (elt feeds n)))
              (select-feed (selector)
                :report (lambda (s) (format s "Provide a function to select the right feed"))
                (find-if selector feeds)))))))))

(defmacro with-retry ((retry-message &rest args) action &body other-restarts)
  `(loop (restart-case (return ,action)
           (retry ()
             :report (lambda (s) (format s ,retry-message ,@args)))
           ,@other-restarts)))

(defun pull-feed (url &key detect type)
  (with-retry ("Retry fetching feed ~a" url)
              (to-feed
               (if detect
                   (fetch-feed-from-url url)
                   (fetch-doc-from-url url)) 
               :type type)
    (skip-feed () (return)
               :report (lambda (s) (format s "Skip fetching feed ~a" url)))))
