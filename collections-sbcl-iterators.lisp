(in-package :collection-class)

#+nil
(defmethod sequence:length ((sequence collection))
  (length (items sequence)))

#+sbcl
(defmethod sb-sequence:length ((sequence collection))
  (length (items sequence)))

#+nil
(defmethod sequence:elt ((sequence collection) index)
  (elt (items sequence) index))

#+sbcl
(defmethod sb-sequence:elt ((sequence collection) index)
  (elt (items sequence) index))

#+nil
(defmethod (setf sequence:elt) (new-value (sequence collection) index)
  (setf (elt (items sequence) index) new-value))

#+sbcl
(defmethod (setf sb-sequence:elt) (new-value (sequence collection) index)
  (setf (elt (items sequence) index) new-value))

#+nil
(defmethod sequence:adjust-sequence ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (when (or initial-element initial-contents)
      (setf (items result)
            (sequence:adjust-sequence (items result) length
                                      :initial-element initial-element
                                      :initial-contents initial-contents)))
    result))

#+sbcl
(defmethod sb-sequence:adjust-sequence ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (when (or initial-element initial-contents)
      (setf (items result)
            (sb-sequence:adjust-sequence (items result) length
                                         :initial-element initial-element
                                         :initial-contents initial-contents)))
    result))

#+nil
(defmethod sequence:make-sequence-like ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (setf (items result)
          (sequence:make-sequence-like (items result) length
                                       :initial-element initial-element
                                       :initial-contents initial-contents))
    result))

#+sbcl
(defmethod sb-sequence:make-sequence-like ((sequence collection) length &key initial-element initial-contents)
  (let ((result (duplicate-collection sequence)))
    (setf (items result)
          (sb-sequence:make-sequence-like (items result) length
                                          :initial-element initial-element
                                          :initial-contents initial-contents))
    result))

