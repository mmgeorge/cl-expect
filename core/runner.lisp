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

(defun make-test-file (&optional (package *package*))
  (let ((package-name (string-downcase (package-name package)))
        (path (test-file-path package)))
    (format t "Writing test file at ~a~%" path)
    (handler-case 
        (with-open-file (ostream (pathname path) :direction :output :if-does-not-exist :create)
          (format ostream "(defpackage :~a.test ~%  (:use :cl :~a))~%~%(in-package :~a.test)~%"
                  package-name package-name package-name))
      (t ()
        (format t "Unable to write file as it already exists!")))))


(defun test-file-path (package)
  (let* ((system (find-current-system))
         (path-base (namestring (asdf:component-pathname system)))
         (package-relative-path (package-relative-path package))
         (extension ".test.lisp"))
    (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
    (concatenate 'string path-base package-relative-path extension)))


(defun package-relative-path (package)
  (let* ((name (package-name package))
         (start (loop for i from 0
                  for char across name until (eq char #\/)
                  finally (return (1+ i)))))
    (string-downcase (subseq name start))))


(defun parent-directory (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))))


(defun find-system-file (directory)
  (flet ((asd-file-p (pathname) (string-equal (pathname-type pathname) "asd")))
    (or (find-if #'asd-file-p (uiop:directory-files directory))
        (find-system-file (parent-directory directory)))))
        

(defun find-current-system ()
  (let ((system-name (pathname-name (find-system-file *default-pathname-defaults*))))
    (asdf:find-system system-name)))


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
