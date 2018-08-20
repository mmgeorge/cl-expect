(defpackage :expect/report/report
  (:use :cl)
  (:export #:report #:make-report #:failed #:children #:has-children-p
           #:summarize #:nested-failed-length  #:record #:print-report
           #:*indent-amount*))

(in-package :expect/report/report)


(defvar *indent-amount* 2)


(defclass report ()
  ((children :accessor children :initform nil :initarg :children)
   (len :accessor len :initarg :len)
   (failed :reader failed :initarg :failed :initform nil)))


(defun make-report (&optional (children nil))
  (make-instance 'report :children children))


(defgeneric record (report child))
(defgeneric print-report (report indent))
(defgeneric summarize (report))
(defgeneric has-children-p (report))


(defmethod record ((report report) child)
  (setf (children report) (cons child (children report))))


(defmethod print-report ((report report) indent)
  (mapcar #'(lambda (child) (print-report child (+ *indent-amount* indent))) (children report)))


(defmethod nested-length ((self report))
  (if (null (children self)) 1
      (reduce #'(lambda (prev child) (+ prev (nested-length child))) (children self) :initial-value 0)))


(defmethod nested-failed-length ((self report))
  (if (failed self) 1
      (reduce #'(lambda (prev child) (+ prev (nested-failed-length child))) (children self) :initial-value 0)))


(defmethod has-children-p ((self report))
  (> (length (children self)) 0))


(defmethod summarize ((self report))
  (let* ((failures (nested-failed-length self))
         (len (nested-length self))
         (result (if (> failures 0) "Ran with failures" "All tests passed")))
    (format t "~%~a: [~a/~a]" result (- len failures) len)))
