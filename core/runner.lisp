(defpackage :expect/runner
  (:use :cl)
  (:import-from :expect/suite)
  (:import-from :expect/test)
  (:import-from :expect/report/report)
  (:import-from :expect/macros #:deftest-of)
  (:import-from :blackbird)
  (:export #:run-tests #:clear-tests #:make-test-file)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)
                    (:report :expect/report/report)))

(in-package :expect/runner)


;; Exports

(defun run-tests (&optional (package-or-name *package*))
  (check-type package-or-name (or package string))
  (print-report
   (if (typep package-or-name 'string)
       (let ((system (find-system package-or-name)))
         (if system
             (run-system system)
             (run-package-suite package-or-name t)))
       (run-package-suite (package-name package-or-name)))))


(defun clear-tests (&optional (package-or-name *package*))
  (check-type package-or-name (or package string))
  (if (system-name-p package-or-name)
      (suite:clear-all)
      (if (suite:suite-exists-p package-or-name)
          (suite:clear (suite:suite-of package-or-name))
          (error "Cannot clear tests of ~a - package does not exist!" package-or-name))))


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


;; Internal

(defun print-report (report)
  (progn (report:print-report report 0)
       (report:summarize report)))

(defun system-name-p (maybe-system-name)
  (and (typep maybe-system-name 'string) (find-system maybe-system-name)))


(defun find-system (name)
  (handler-case (asdf:find-system name)
    (t () nil)))

(defun run-system (system)
  (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
  (let* ((children (gather-children system))
         (names (mapcar #'asdf:component-name children))
         (reports (remove-if-not #'(lambda (report) (and report (report:has-children-p report) ))
                                 (mapcar (lambda (name) (run-package-suite name nil)) names))))
         (report:make-report reports)))


(defun run-package-suite (package-name &optional (must-exist t))
  (load-suite-test-file package-name must-exist)
  (when (suite:suite-exists-p package-name)
    (let ((report (suite:run (suite:suite-of package-name))))
      report)))


(defun load-suite-test-file (package &optional (must-exist t))
  "Load the suite for the given PACKAGE. If MUST-EXIST is true, the function will throw an error if 
unable to load the suite"
  ;; TODO - UGLY! loading should be an internal method of suite
  (handler-case 
   (let* ((path (test-file-path package))
          (timestamp (file-write-date path)))
     (if (suite:suite-exists-p package)
         (unless (eq (suite:load-timestamp (suite:suite-of package)) timestamp)
           ;; Warning! This assumes definitions are not split between PACKAGE FILE and
           ;; PACKAGE TEST FILE which they technically could be.
           (clear-tests package)
           (setf (suite:load-timestamp (suite:suite-of package)) timestamp)
           (load path))
         (progn
           (setf (suite:load-timestamp (suite:suite-of package)) timestamp)
           (load path))))
    (error (e)
      (when (and must-exist (not (suite:suite-exists-p package)))
        (error "Encountered an error duirng loading suite test file. Does the file exists?~%  Create a new test file for the current package with (expect:make-test-file). ~%~%Full error: ~%~a" e)))))


(defun gather-children (system &optional (visited nil))
  ;; A "proper-dep" is a dependency with the same primary system name (i.e., not an external library)
  (flet ((proper-dep-p (dep) (string-equal (asdf:primary-system-name dep) (asdf:primary-system-name system)))
         (recurse (prev dep) (gather-children dep prev)))
    (let* ((proper-deps (remove-if-not #'proper-dep-p (mapcar #'asdf:find-system (asdf:system-depends-on system))))
           (unvisited-deps (set-difference proper-deps visited :test #'component-equal)))
      (reduce #'recurse unvisited-deps :initial-value (append unvisited-deps visited)))))


(defun test-file-path (package)
  (let* ((system (find-current-system))
         (path-base (namestring (asdf:component-pathname system)))
         (package-relative-path (package-relative-path package))
         (extension ".test.lisp"))
    (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
    (concatenate 'string path-base package-relative-path extension)))


(defun package-relative-path (package)
  (let* ((name (string-downcase (if (typep package 'string) package (package-name package))))
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


(defun component-equal (component other)
  (string-equal (asdf:component-name component)
                (asdf:component-name other)))
