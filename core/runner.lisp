(defpackage :expect/runner
  (:use :cl)
  (:import-from :expect/suite)
  (:import-from :expect/test)
  (:import-from :expect/report/report)
  (:import-from :expect/macros #:deftest-of)
  (:import-from :blackbird)
  (:export #:run #:run-tests #:clear-tests)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)
                    (:report :expect/report/report)))

(in-package :expect/runner)


(defun run-tests (&optional (package-or-system-name *package*))
  (if (typep package-or-system-name 'string)
      (run package-or-system-name)
      (if (not (suite:suite-exists-p package-or-system-name))
          (format t "No tests to run")
          (let ((report (suite:run (suite:suite-of package-or-system-name))))
            (report:print-report report 0)
            (report:summarize report))))
  nil)


(defun clear-tests (&optional (package-or-system-name *package*))
  (if (typep package-or-system-name 'string)
      (run package-or-system-name)
      (if (not (suite:suite-exists-p package-or-system-name))
          (format t "No tests to remove")
          (suite:clear (suite:suite-of package-or-system-name))))
  nil)


(defun run (system-name)
  (check-type system-name string)
  (let ((system (asdf:find-system system-name)))
    (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
    (run-system system)))


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))
  

(defun run-system (system)
  (let* ((children (gather-children system))
         (names (mapcar #'asdf:component-name children))
         (reports (remove-if-not #'report:has-children-p 
                                 (mapcar (lambda (name) (suite:run (suite:suite-of name))) names)))
         (report (report:make-report reports)))
    (if (> (length reports) 0)
        (progn (report:print-report report 0)
               (report:summarize report))
        (format t "No tests specified"))))


(defun gather-children (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (gather-children dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))
