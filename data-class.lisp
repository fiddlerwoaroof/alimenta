(in-package #:data-class)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-channel-element (xml-dom el)
    ($ (inline xml-dom) el (text) (node))))

(defun element-name-from-symbol (sym)
  (let* ((base (string-downcase sym))
         (split (split-sequence:split-sequence #\~ base))
         (capit (cons (car split) (map 'list #'nstring-capitalize (cdr split)))))
    (apply #'concatenate 'string capit)))

(defmacro ensure-slot (sym &body or-else)
  `(handler-case
     ,sym
     (unbound-slot (c)
       (declare (ignore c))
       ,@or-else)))

(defmacro lazy-load-slot (class-name doc-slot root-el name tag-name &key transform value)
  `(defmethod ,name :before ((self ,class-name))
     (with-slots (,name ,doc-slot) self
       (ensure-slot ,name
         (alet ,(or value
                    `(get-channel-element ,doc-slot
                                          ,(format nil "~a > ~a" root-el tag-name)))
           ,(if transform
              `(setf ,name (when it (,transform it)))
              `(setf ,name it)))))))

(defgeneric all-slots (self format))

(defmacro define-data-class (name (doc-slot root-el) (&rest superclasses) &body slots)
  `(progn
     (defclass ,name ,superclasses
       ,(loop for (slot) in (fw.lu:ensure-mapping slots)
              collect `(,slot :initarg ,(make-keyword slot) :accessor ,slot)))
     ,@(loop for (slot tag-name . rest) in (fw.lu:ensure-mapping slots)
             collect `(lazy-load-slot ,name ,doc-slot ,root-el ,slot ,tag-name ,@rest))
     (defmethod all-slots ((self ,name) format)
       (pairlis (list ,@(mapcar (fw.lu::alambda (make-keyword (car it)))
                                (fw.lu:ensure-mapping slots)))
                (list ,@(loop for (slot) in (fw.lu:ensure-mapping slots)
                              collect `(,slot self)))))))




