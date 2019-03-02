(defpackage :expect/util
  (:use :cl :blackbird)
  (:export #:amap-wait #:get-current-system #:gather-children))

(in-package :expect/util)


(defun amap-wait (function list)
  "Run map over a list of promises, finishing the returned promises once all values 
have been fullfilled. Unlike amap, amap-wait waits on each individual promise before 
executing the next in the sequence, ensuring promise is resolved in turn."
  (bb:all
   (reduce #'(lambda (promise-list curr)
               (cons
                (bb:wait (car promise-list)
                      (funcall function curr))
                promise-list)) list :initial-value nil)))


(defun get-current-system ()
  ;; What if called outside a given system? 
  (labels ((recurse (pathname)
             (or (directory (make-pathname :name :wild :type "asd" :defaults pathname))
                 (recurse (make-pathname :directory (butlast (pathname-directory pathname)))))))
    (asdf:find-system (pathname-name (car (funcall #'recurse (uiop:getcwd)))))))


(defun gather-children (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (gather-children dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))
