(defpackage :expect/suite
  (:use :cl)
  (:import-from :expect/test)
  (:export #:suite-of #:register #:tests #:run)
  (:local-nicknames (:test :expect/test)))

(in-package :expect/suite)


(defvar *suites* (make-hash-table :test 'eq))


(defclass suite ()
  ((package :accessor package-of :initarg :package)
   (tests :accessor tests :initform (make-hash-table :test 'equal)  :type 'hash-table)))


(defun make-suite (package)
  (make-instance 'suite :package package))


(defun register (self test)
  (when (gethash (test:name test) (tests self))
    (format t "Redefining test definition for ~a" (test:name test)))
  (setf (gethash (test:name test) (tests self)) test))


(defun suite-of (package)
  (let ((*print-case* :downcase))
    
  (or (gethash package *suites*)
      (setf (gethash package *suites*)
            (make-suite package)))))


(defun run (self)
  (let ((failures
          (loop for test being the hash-values of (tests self)
                for failures = (test:run test)
                if failures
                  collect (list test failures))))
    (mapcar #'(lambda (failure) (print-failure self failure)) failures)))


(defun print-failure (self test-failure)
  (let ((*print-case* :downcase))


  (destructuring-bind (test failures) test-failure
    (format t "Test ~a:~a had [~a] failures:~%"
            (package-name (package-of self)) (test:name test) (length failures))
    (loop for failure in failures
          for i from 1 do
      (progn
        (format t "  [~a] " i)
        (test:print-result failure)))
    (format t "~%")
    
  )))


