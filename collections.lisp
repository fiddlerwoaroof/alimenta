(in-package :collection-class)

(defclass collection (standard-object sequence)
  ())

(define-condition value-error ()
  ((value :initarg :value :accessor value)))

(defgeneric push-item (collection item)
  (:documentation "Push item onto the beginning of the collection"))

(defgeneric items (collection)
  (:documentation "Get the items from a collection"))

(defgeneric duplicate-collection (collection))

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

                   An uninitialized object of the same class as OBJECT is allocated by
                   calling ALLOCATE-INSTANCE.  For all slots returned by
                   CLASS-SLOTS, the returned object has the
                   same slot values and slot-unbound status as OBJECT.

                   REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
   (let* ((class (class-of object))
          (copy (allocate-instance class)))
     (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
       (when (slot-boundp object slot-name)
         (setf (slot-value copy slot-name)
               (slot-value object slot-name))))
     (apply #'reinitialize-instance copy initargs))))


; TODO: actually use item-class...
; TODO: finish initform handling.  Have to figure out how to make initform work with push-item
(defmacro define-collection ((name item-class &key (initform '(list))) (&rest supers) &body ((&rest slots) &rest other-stuff))
  (with-gensyms (item-slot-sym)
    `(progn (defclass ,name (,@supers collection)
              ((,item-slot-sym :initform ,initform :accessor items)
               ,@slots)
              ,@other-stuff)
            (defmethod duplicate-collection ((collection ,name))
              (let ((result (copy-instance collection)))
                (setf (items result)
                      (copy-seq (items result)))
                result))
            (defmethod push-item ((collection ,name) (item ,item-class))
              (push item (items collection))))))

(defclass collection-iterator (for:iterator)
  ())

(defmethod initialize-instance :after ((iterator collection-iterator) &key object)
  (setf (for:object iterator)
        (items object)))

(defmethod random-item ((collection collection) &optional (random-state *random-state*))
  (let* ((length (length (items collection)))
         (selected-index (random length random-state)))
    (elt (items collection)
         selected-index)))

(defmethod nth-item ((collection collection) (index integer))
  (if (>= index 0)
    (elt (items collection) index)
    (error 'value-error :value index)))
