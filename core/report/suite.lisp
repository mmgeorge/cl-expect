(defpackage :expect/report/suite
  (:use :cl)
  (:import-from :expect/report/report)
  (:export #:suite #:make-suite)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/suite)


(defclass suite (report:report)
  ()
  )


(defun make-suite ()
  (make-instance 'suite))


(defmethod report:print-report ((self suite) indent)
  (call-next-method))

