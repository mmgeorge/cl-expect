(defpackage :expect/report/suite
  (:use :cl)
  (:import-from :expect/report/report)
  (:export #:suite #:make-suite)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/suite)


(defclass suite (report:report)
  ((suite-name :reader suite-name :initarg :suite-name)))


(defun make-suite (len suite-name)
  (make-instance 'suite :len len :suite-name suite-name))


(defmethod report:nested-length ((self suite))
  (length (report:children self)))


(defmethod report:print-report ((self suite) indent)
  (let ((*print-case* :downcase))
    (call-next-method)))

