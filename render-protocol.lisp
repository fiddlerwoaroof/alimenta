(in-package :alimenta.render)

(defgeneric render-feed (renderer feed)
  (:documentation "Render the container for the feed's items. Return an object
                   to which the items can be added via add-rendered-item"))

(defgeneric render-item (renderer item feed)
  (:documentation "Render an item to be added to a feed. Return an object that
                   can be added to the container by add-rendered-item"))

(defgeneric add-rendered-item (renderer feed-representation item-representation)
  (:documentation "Add the rendered item to the rendered feed"))

(defmethod alimenta.render:render-feed ((renderer (eql :hash-table)) (feed alimenta:feed))
  (alexandria:plist-hash-table
   (list "title" (alimenta:title feed)
         "link" (alimenta:link feed)
         "description" (alimenta:description feed)
         "url" (puri:render-uri (alimenta:feed-link feed) nil)
         "source-type" (string (alimenta:source-type feed)))
   :test 'equal))

(defmethod alimenta.render:render-item ((renderer (eql :hash-table))
                                        (item alimenta:item)
                                        (feed alimenta:feed))
  (alexandria:plist-hash-table
   (list "title" (alimenta:title item)
         "link" (alimenta:link item)
         "author" (alimenta:author item)
         "date" (local-time:format-rfc3339-timestring nil (alimenta:date item))
         "id" (alimenta:id item)
         "description" (alimenta:description item)
         "content" (alimenta:content item))
   :test 'equal))

(defmethod alimenta.render:add-rendered-item ((renderer (eql :hash-table))
                                              (feed-representation hash-table)
                                              (item hash-table))
  (prog1 feed-representation
    (setf (gethash "items"
                   feed-representation
                   nil)
          (append (gethash "items"
                           feed-representation
                           nil)
                  (list item)))))
