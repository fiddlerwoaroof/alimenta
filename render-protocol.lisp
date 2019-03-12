(in-package :alimenta.render)

(defgeneric render-feed (feed renderer)
  (:documentation "Render the container for the feed's items. Return an object
                   to which the items can be added via add-rendered-item"))

(defgeneric render-item (item feed renderer)
  (:documentation "Render an item to be added to a feed. Return an object that
                   can be added to the container by add-rendered-item"))

(defgeneric add-rendered-item (feed-representation item-representation renderer)
  (:documentation "Add the rendered item to the rendered feed"))
