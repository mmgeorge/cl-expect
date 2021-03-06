(defpackage :expect/report/report
  (:use :cl)
  (:export #:report #:make-report #:failed #:children #:has-children-p
           #:write-to-output
           #:summarize #:nested-failed-length #:nested-length  #:record #:print-report
           #:*indent-amount*))

(in-package :expect/report/report)


(defvar *indent-amount* 2)


(defclass report ()
  ((children :accessor children :initform nil :initarg :children)
   (len :accessor len :initarg :len)
   (failed :accessor failed :initarg :failed :initform nil)))


(defun make-report (&optional (children nil))
  (make-instance 'report :children children))


(defgeneric record (report child))
(defgeneric print-report (report stream indent))
(defgeneric summarize (report))
(defgeneric has-children-p (report))
(defgeneric nested-failed-length (report))
(defgeneric nested-length (report))
(defgeneric write-to-output (report output))


(defmethod record ((report report) child)
  (setf (children report) (nconc (children report) (list child))))


(defmethod print-report ((report report) stream indent)
  (mapcar #'(lambda (child) (print-report child stream indent)) (children report)))


(defmethod nested-length ((self report))
  (if (null (children self)) 1
      (reduce #'(lambda (prev child) (+ prev (nested-length child))) (children self) :initial-value 0)))


(defmethod nested-failed-length ((self report))
  (cond ((failed self) 1)
        (t (reduce #'(lambda (prev child) (+ prev (nested-failed-length child)))
                   (children self) :initial-value 0))))


(defmethod has-children-p ((self report))
  (> (length (children self)) 0))


(defmethod summarize ((self report))
  (let* ((failures (nested-failed-length self))
         (len (nested-length self))
         (result (if (> failures 0) "Ran with failures" "All tests passed")))
    (if (eq len 0)
        (format t "No tests found.~%")
        (format t "~%~a: [~a/~a]~%" result (- len failures) len))))
