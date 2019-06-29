(in-package :wizard)

(defun new-vec ()
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))

(defun vec-to-string (vec)
  (coerce vec 'string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cat (s1 s2)
    (concatenate 'string (string s1) (string s2))))
