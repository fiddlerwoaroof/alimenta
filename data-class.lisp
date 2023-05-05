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

(defgeneric %all-slots (self format))
(defun all-slots (self &optional format)
  (%all-slots self format))

(defgeneric slot-tags (self))

(defun process-slots-for-data-class (slots)
  (mapcar (fw.lu::destructuring-lambda ((slot tag . rest))
            (let ((tag (etypecase tag
                         (symbol (string-downcase tag))
                         (string tag))))
              (list* slot (make-keyword slot) tag rest)))
          (fw.lu:ensure-mapping slots)))

(deftest process-slots-for-data-class ()
  (let ((tc-1 '(a))
        (tc-2 '((a b)))
        (tc-3 '((a "b")))
        (tc-4 '((a b c)))
        (tc-5 '((a "b" c)))
        (tc-6 '((a "bC")))
        (tc-7 '((a "bC" d))))
    (should be equal '((a :a "a")) (process-slots-for-data-class tc-1))
    (should be equal '((a :a "b")) (process-slots-for-data-class tc-2))
    (should be equal '((a :a "b")) (process-slots-for-data-class tc-3))
    (should be equal '((a :a "b" c)) (process-slots-for-data-class tc-4))
    (should be equal '((a :a "b" c)) (process-slots-for-data-class tc-5))
    (should be equal '((a :a "bC")) (process-slots-for-data-class tc-6))
    (should be equal '((a :a "bC" d)) (process-slots-for-data-class tc-7))))

(defmacro define-data-class (name (doc-slot root-el) (&rest superclasses) &body slots)
  (declare (optimize (debug 3)))
  (flet ((make-slot-spec (slot slot-keyword)
           `(,slot :initarg ,slot-keyword :accessor ,slot)))
    (let ((slots (process-slots-for-data-class slots)))
      `(progn
         (defclass ,name ,superclasses
           ((slot-tags :allocation :class :initform
                       ',(loop for (_ slot-keyword tag) in slots
                               collect (cons slot-keyword tag)))
            ,@(mapcar (fw.lu::destructuring-lambda ((slot slot-keyword . r))
                        (declare (ignore r))
                        (make-slot-spec slot slot-keyword))
                      slots)))
         ,@(loop for (slot _ tag-name . rest) in slots
                 collect `(lazy-load-slot ,name ,doc-slot ,root-el ,slot ,tag-name ,@rest))
         (defmethod %all-slots ((self ,name) format)
           (pairlis (list ,@(mapcar (fw.lu::alambda (cadr fw.lu::it)) slots))
                    (list ,@(loop for (slot) in slots collect `(,slot self)))))))))
