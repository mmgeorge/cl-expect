(defpackage :expect/report/suite
  (:use :cl)
  (:import-from :expect/report/report)
  (:export #:suite #:make-suite)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/suite)


(defclass suite (report:report) ())


(defun make-suite (len)
  (make-instance 'suite :len len))


(defmethod report:print-report ((self suite) indent)
  (let ((*print-case* :downcase))
    (call-next-method)))

