(in-package :collection-class)

(defmethod sb-sequence:length ((sequence collection))
  (length (items sequence)))

(defmethod sb-sequence:elt ((sequence collection) index)
  (elt (items sequence) index))

(defmethod (setf sb-sequence:elt) (new-value (sequence collection) index)
  (setf (elt (items sequence) index) new-value))

(defmethod sb-sequence:adjust-sequence ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (when (or initial-element initial-contents)
      (setf (items result)
            (sb-sequence:adjust-sequence (items result) length
                                         :initial-element initial-element
                                         :initial-contents initial-contents)))
    result))

(defmethod sb-sequence:make-sequence-like ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (setf (items result)
          (sb-sequence:make-sequence-like (items result) length
                                          :initial-element initial-element
                                          :initial-contents initial-contents))
    result))

