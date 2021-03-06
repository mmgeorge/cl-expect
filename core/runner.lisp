(defpackage :expect/runner
  (:use :cl :blackbird)
  (:import-from :expect/suite)
  (:import-from :expect/test)
  (:import-from :expect/report/report)
  (:import-from :expect/macros)
  (:import-from :expect/util)
  (:import-from :blackbird)
  (:export #:run-tests #:clear-tests #:make-test-file)
  (:local-nicknames
    (:suite :expect/suite)
    (:util :expect/util)
    (:test :expect/test)
    (:report :expect/report/report)))

(in-package :expect/runner)


;; Exports

(defun run-tests (&optional (package-or-name *package*) (return-promise nil))
  (check-type package-or-name (or package string))
  (let ((promise
          (catcher 
           (alet ((report 
                   (if (typep package-or-name 'string)
                       (let ((system (find-system package-or-name)))
                         (if system
                             (run-system system)
                             (run-package-suite package-or-name t)))
                       (run-package-suite (package-name package-or-name)))))
                 (print-report report)
                 report)
           ;; TODO - clean this up! We would like to throw certain errors that occur duing
           ;; file loading, however, this is currently clobbering those errors. 
           (t (e)
              (format t "~%Encountered a FATAL ERROR when running tests. ~%~
                         This likely indicates a bug in cl-expect:~2%~a~2%"
                      (dissect:environment-condition e))
              (format t "~{~a~%~}" (dissect:environment-stack e))))))
    (when return-promise promise)))




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
    (handler-case 
        (with-open-file (ostream (pathname path) :direction :output :if-does-not-exist :create)
          (format ostream "(defpackage :~a.test ~%  (:use :cl :~a))~%~%(in-package :~a.test)~%"
                  package-name package-name package-name))
      (t (e)
        (error "Unable to create test file as it already exists!~%~% Details:~% ~a" e)))))


;; Internal

(defun print-report (report)
  (progn (report:print-report report *standard-output* 0)
       (report:summarize report)))


(defun system-name-p (maybe-system-name)
  (and (typep maybe-system-name 'string) (find-system maybe-system-name)))


(defun find-system (name)
   (asdf:find-system name nil))
    

(defun run-system (system)
  (check-type system asdf:package-inferred-system "Expect expects a package-inferred-system")
  (let* ((children (expect/util:gather-children system))
         (names (mapcar #'asdf:component-name children))
         (report-promises (bb:afilter #'(lambda (report) (and report (report:has-children-p report) ))
                                      (util:amap-wait (lambda (name)
                                                         (run-package-suite name nil)) names))))
    (alet ((reports (all report-promises)))
      (report:make-report reports))))


(defun run-package-suite (package-name &optional (must-exist t))
  (load-suite-test-file package-name must-exist)
  (if (suite:suite-exists-p package-name)
      (suite:run (suite:suite-of package-name))
      (bb:promisify nil)))


(defun load-suite-test-file (package &optional (must-exist t))
  "Load the suite for the given PACKAGE. If MUST-EXIST is true, the function will throw an error if 
unable to load the suite"
  ;; TODO - UGLY! loading should be an internal method of suite
  (handler-case
      (let* ((expect/macros:*print-compile-message* nil)
             (path (test-file-path package)))
        (if (probe-file path)
            (let ((timestamp (file-write-date path)))
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
            (and must-exist (error "Unable to load suite test file. Does the file exists?~%  Create a new test file for the current package with (expect:make-test-file)."))))
    (error (e)
      (format t "~%Load Error: ~a~%" e))))
      


(defun test-file-path (package)
  (let* ((system (expect/util:get-current-system))
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
