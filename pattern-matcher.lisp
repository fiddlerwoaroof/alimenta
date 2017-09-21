(defpackage :alimenta%pattern-matcher
  (:use :cl :patmatch :alimenta)
  (:export ))
(in-package :alimenta%pattern-matcher)


(defmethod handle-pattern append ((pattern feed-entity) form &rest args)
  (let ((key->reader '((:title . title)
                       (:link . link)
                       (:doc . doc))))
    (let* ((val-sym (gensym "VAL"))
           (binders (loop for (key binding) on args by #'cddr
                       for accessor = (cdr (assoc key key->reader))
                       when accessor append
                         `((,binding (,accessor ,val-sym))))))
      `((,val-sym ,form)
        ,@binders))))

(defmethod handle-pattern append ((pattern feed) form &rest args)
  (let ((key->reader '((:author . author)
                       (:items . collection-class:items)
                       (:description . description)
                       (:source-type . source-type)
                       (:feed-link . feed-link))))
    (let* ((val-sym (gensym "VAL"))
           (binders (loop for (key binding) on args by #'cddr
                       for accessor = (cdr (assoc key key->reader))
                       when accessor append
                         `((,binding (,accessor ,val-sym))))))
      `((,val-sym ,form)
        ,@binders))))

(defmethod handle-pattern append ((pattern item) form &rest args)
  (let ((key->reader '((:author . author)
                       (:content . content)
                       (:date . date)
                       (:id . id)
                       (:links . links))))
    (let* ((val-sym (gensym "VAL"))
           (binders (loop for (key binding) on args by #'cddr
                       for accessor = (cdr (assoc key key->reader))
                       when accessor append
                         `((,binding (,accessor ,val-sym))))))
      `((,val-sym ,form)
        ,@binders))))

