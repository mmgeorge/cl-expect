(defpackage :expect/suite
  (:use :cl)
  (:import-from :expect/test)
  (:import-from :expect/report/report #:record)
  (:import-from :expect/report/suite)
  (:export #:suite-of #:suite-exists-p #:register #:tests #:run)
  (:local-nicknames (:test :expect/test)
                    (:report :expect/report/report)
                    (:report/suite :expect/report/suite)))

(in-package :expect/suite)


(defvar *suites* (make-hash-table :test 'equal))


(defclass suite ()
  ((package-of :accessor package-of :initarg :package)
   (tests :accessor tests :initform (make-hash-table :test 'equal)  :type 'hash-table)))


(defun make-suite (package)
  (make-instance 'suite :package package))


(defun register (self test)
  (let ((tests (gethash (test:name test) (tests self))))
    (format t "Adding test definition for ~a" (test:name test))
    (setf (gethash (test:name test) (tests self))
          (if tests 
              (append tests (list test))
              (list test)))))


(defun suite-exists-p (package)
  (let ((name (string-downcase (if (typep package 'string) package (package-name package)))))
    (gethash name *suites*)))


(defun suite-of (package)
  (let ((name (string-downcase (if (typep package 'string) package (package-name package)))))
    (or (gethash name *suites*)
        (setf (gethash name *suites*)
              (make-suite name)))))


(defun run (self)
  (let ((report (report/suite:make-suite (hash-table-count (tests self)))))
    (loop for function-tests being the hash-values of (tests self) do
      (loop for test in function-tests
            for test-report = (test:run test) do
              (record report test-report)))
    report))
