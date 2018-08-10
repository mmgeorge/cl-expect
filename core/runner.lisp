(defpackage :expect/runner
  (:use :cl)
  (:import-from :expect/suite)
  (:import-from :expect/test)
  (:import-from :expect/macros #:deftest-of)
  (:import-from :blackbird)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test))
  (:export #:run))

(in-package :expect/runner)


(defun run (system-name)
  (check-type system-name string)
  (let ((system (asdf:find-system system-name)))
    (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
    (run-system system)))


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))
  

(defun run-system (system)
  (let ((children (gather-children system)))
    children

    ))


(defvar run-system nil)


  
 ;; (deftest-of run-system ()
 ;;   (with-fixtures (test-system)
 ;;     (expect (equal (run-system 1) 1))
 ;;     (expect (equal (run-system 12) 12)))

  
;; (expect (run-system 1)) result)
;; (expect (run-system 1))  result)

(defun gather-children (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (gather-children dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))
    
