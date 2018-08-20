(defpackage :expect/suite
  (:use :cl)
  (:import-from :expect/test)
  (:import-from :expect/report/report #:record)
  (:import-from :expect/report/suite)
  (:export #:suite-of #:register #:tests #:run)
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
  (when (gethash (test:name test) (tests self))
    (format t "Redefining test definition for ~a" (test:name test)))
  (setf (gethash (test:name test) (tests self)) test))


(defun suite-of (package)
  (let ((name (string-downcase (if (typep package 'string) package (package-name package)))))
    (or (gethash name *suites*)
        (setf (gethash name *suites*)
              (make-suite name)))))


(defun run (self)
  (let ((report (report/suite:make-suite)))
    (loop for test being the hash-values of (tests self)
          for test-report = (test:run test)
          do (record report test-report))
    report))

    
;    (when print-failures
 ;     (mapcar #'(lambda (failure) (print-failed-test self failure)) failures))
    ;failures))
