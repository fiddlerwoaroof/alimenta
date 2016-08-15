(defmethod for:has-more ((iterator collection-iterator))
  (not (null (for:object iterator))))

(defmethod for:next ((iterator collection-iterator))
  (let ((collection-items (for:object iterator)))
    (prog1 (car collection-items)
      (setf (for:object iterator)
            (cdr collection-items)))))

(defmethod for:make-iterator ((collection collection) &key)
  (make-instance 'collection-iterator :object collection))


