(defpackage :expect/suite
  (:use :cl)
  (:import-from :expect/test)
  (:import-from :expect/report/report #:record)
  (:import-from :expect/report/suite)
  (:export #:suite-of #:suite-exists-p #:register #:clear #:clear-all #:tests #:run #:load-timestamp)
  (:local-nicknames (:test :expect/test)
                    (:report :expect/report/report)
                    (:report/suite :expect/report/suite)))

(in-package :expect/suite)


(defvar *suites* (make-hash-table :test 'equal))


(defclass suite ()
  ((package-of :accessor package-of :initarg :package)
   (tests :accessor tests :initform (make-hash-table :test 'equal)  :type 'hash-table)
   (load-timestamp :accessor load-timestamp :initform nil)))


(defun make-suite (package)
  (make-instance 'suite :package package))


(defun register (self test)
  (let ((tests (gethash (test:name test) (tests self))))
    (format t "Adding test definition for ~a~%" (test:name test))
    (setf (gethash (test:name test) (tests self))
          (if tests
              (insert-or-replace-test test tests)
              (list test)))))


(defun insert-or-replace-test (test list-of-tests)
  (flet ((same-test-p (target)
           (and (string-equal (test:suite-name target) (test:suite-name test))
                (string-equal (test:name target) (test:name test))
                (string-equal (test:description target) (test:description test)))))
    (let ((pos (position-if #'same-test-p list-of-tests)))
      (if pos
          (setf (nth pos list-of-tests) test)
          (nconc list-of-tests (list test)))
      list-of-tests)))


(defun clear (self)
  (format t "Removing ~a tests for ~a" (hash-table-count (tests self)) (package-of self))
  (setf (tests self) (make-hash-table :test 'equal)))


(defun clear-all ()
  (setf *suites* (make-hash-table :test 'equal)))


(defun name (package)
  (let* ((package-name (string-downcase (if (typep package 'string) package (package-name package))))
         (len (length package-name)))
    (if (and (> len 5) (string-equal (subseq package-name (- len 5) len) ".test"))
        (subseq package-name 0 (- len 5))
        package-name)))


(defun suite-exists-p (package)
  (let ((name (name package)))
    (gethash name *suites*)))


(defun suite-of (package)
  (let  ((name (name package)))
    (or (gethash name *suites*)
        (setf (gethash name *suites*)
              (make-suite name)))))


(defun run (self)
  (let ((report (report/suite:make-suite (hash-table-count (tests self)) (package-of self)))
        (suite-name (package-of self)))
    (loop for function-tests being the hash-values of (tests self) using (hash-key test-name) do
      (let ((result-stream (make-string-output-stream))
            (passed-test-count 0))
        (loop for test in function-tests
              for count = (length (test:expects test))
              for desc = (test:description test)
              for test-report = (test:run test) do
                (if (> (report:nested-failed-length test-report) 0)
                    (format result-stream "   - ~a [~a/~a]~%" desc
                            (- count (report:nested-failed-length test-report)) count)
                    (incf passed-test-count))
                (record report test-report))
        (let* ((test-count (length function-tests))
               (result (if (eq passed-test-count test-count) "PASS" "FAIL")))
        (format t "[~a] ~a:~a [~a/~a]~%" result suite-name test-name passed-test-count test-count))))
    report))

