(defpackage :expect/report/test
  (:use :cl)
  (:import-from :expect/report/report #:failed)
  (:export #:test #:make-test)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/test)


(defclass test (report:report)
  ((name :reader name :initarg :name)
   (description :reader description :initarg :description)
   (suite-name :reader suite-name :initarg :suite-name)))


(defun make-test (name suite-name description)
  (make-instance 'test :name name :suite-name suite-name :description description))


(defmethod report:print-report ((self test) indent)
  (when (find-if #'failed (report:children self))
    (format t "~%")
    (format t "~Va ~a:~a ~%~Va ~a had [~a] failures:~%"
            indent "Test of" (suite-name self) (name self) indent "" (description self) (report:nested-failed-length self))
    (call-next-method)))
