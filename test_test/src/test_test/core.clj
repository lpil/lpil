(ns test_test.core)

;;; This is an incorrect implementation, such as might be written by
;;; someone who was used to a Lisp in which an empty list is equal to
;;; nil.
; (defn first-element [sequence default]
;   (if (nil? sequence)
;     default
;     (first sequence)))

;;; Hey look, a better one!
(defn first-element [collection default]
  (if (empty? collection)
    default
    (first collection)))
