(in-package :alimenta.discover)

(define-lquery-function get-alternates (node)
  ($ (inline node) "link[rel=alternate]"
     (combine (attr "type") (attr "href"))))

(defun transform-nodes (feed-info-nodes)
  (map 'list (lambda (x) (cons (make-keyword (string-upcase (car x))) (cadr x)))
       feed-info-nodes))

(defgeneric discover-feed (doc))
(defmethod discover-feed ((doc string))
       (transform-nodes ($ (initialize doc) (get-alternates) (node))))
(defmethod discover-feed ((doc plump:node))
       (transform-nodes ($ (inline doc) (get-alternates) (node))))

