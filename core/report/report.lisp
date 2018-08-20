(defpackage :expect/report/report
  (:use :cl)
  (:export #:report #:children #:record #:print-report #:*indent-amount*))

(in-package :expect/report/report)


(defvar *indent-amount* 2)


(defclass report ()
  ((children :accessor children :initform nil)))


(defgeneric record (report child))
(defgeneric print-report (report indent))


(defmethod record ((report report) child)
  (setf (children report) (cons child (children report))))


(defmethod print-report ((report report) indent)
  (mapcar #'(lambda (child) (print-report child (+ *indent-amount* indent))) (children report)))

