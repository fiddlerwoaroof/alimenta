(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :alimenta.pull-feed)

(defmacro setup-libraries-for-feeds (&body body)
  `(let ((plump:*tag-dispatchers* plump:*xml-tags*)
         (drakma:*text-content-types* 
           (pairlis '("application" "application")
                    '("atom+xml"    "rss+xml")
                    drakma:*text-content-types*)))
     ,@body))

(defun fetch-doc-from-url (url)
  (setup-libraries-for-feeds 
    (plump:parse (drakma:http-request url))))

(define-condition fetch-error () ())
(define-condition feed-ambiguous (fetch-error) ((choices :initarg :choices :initform nil)))
(define-condition no-feed (fetch-error) ((url :initarg :url :initform nil)))

(defun feed-ambiguous (choices)
  (error 'feed-ambiguous
         :choices choices))

(defun no-feed (url)
  (cerror "Skip this feed" 'no-feed :url url))

(defun fetch-feed-from-url (url &key type)
  (setup-libraries-for-feeds
    (let* ((feeds (alimenta.discover:discover-feed (drakma:http-request url)))
           (feeds (if type (remove-if-not (lambda (x) (eql type (car x))) feeds) feeds)))
      (if (not feeds) (no-feed url)
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

(defun pull-feed (url &key detect type)
  (to-feed
    (if detect
      (fetch-feed-from-url url)
      (fetch-doc-from-url url)) 
    :type type))
