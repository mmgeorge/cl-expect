(defpackage :expect/suite
  (:use :cl)
  (:import-from :expect/test)
  (:export #:suite-of #:register #:tests #:run)
  (:local-nicknames (:test :expect/test)))

(in-package :expect/suite)


(defvar *suites* (make-hash-table :test 'equal))


(defclass suite ()
  ((package :initarg :package)
   (tests :accessor tests  :initform (make-hash-table)  :type 'hash-table)))


(defun make-suite (package)
  (make-instance 'suite :package package))


(defun register (self test)
  (when (gethash (test:name test) (tests self))
    (format t "Redefining test definition for ~a" (test:name test)))
  (setf (gethash (test:name test) (tests self)) test))


(defun suite-of (package)
  (or (gethash (package-name package) *suites*)
      (setf (gethash (package-name package) *suites*)
            (make-suite package))))


(defun run (self)
  (loop for test being the hash-values of (tests self) do
        (test:run test)))
