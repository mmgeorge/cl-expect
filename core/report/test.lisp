(defpackage :expect/report/test
  (:use :cl)
  (:import-from :expect/report/report)
  (:import-from :expect/report/expect #:failed)
  (:export #:test #:make-test)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/test)


(defclass test (report:report)
  ((name :reader name :initarg :name)
   (suite-name :reader suite-name :initarg :suite-name)))


(defun make-test (name suite-name)
  (make-instance 'test :name name :suite-name suite-name))


(defmethod report:print-report ((self test) indent)
  (when (find-if #'failed (report:children self))
    (format t "~%")
    (format t "~Va ~a:~a had [~a] failures:~%"
            indent "Test" (suite-name self) (name self) (length (report:children self)))
    (call-next-method)))
