(defpackage :expect/report/expect
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/report/report #:failed)
  (:import-from :expect/report/dump #:print-failed-env)
  (:export #:expect #:make-expect #:ref #:lhs #:rhs)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/expect)


(defclass expect (report:report)
  ((uneval :reader uneval :initarg :uneval)
   (predicate :reader predicate :initarg :predicate)
   (form :reader form :initarg :form)
   (expected :reader expected :initarg :expected)
   (lhs :reader lhs :initarg :lhs)
   (rhs :reader rhs :initarg :rhs)))


(defun make-expect (failed uneval pred form expected lhs rhs)
  (make-instance 'expect :failed failed :uneval uneval :predicate pred :form form :expected expected :lhs lhs :rhs rhs))


(defmethod report:print-report ((self expect) indent)
  (when (failed self)
    (with-accessors ((uneval uneval) (predicate predicate) (form form) (expected expected)) self
      ;;(format t "~V@a ~a -> (~s ~s ~s)~%" indent "-" uneval predicate form expected)
      (format t "~V@a (~s ~s ~s)~%" indent "-" predicate form expected)
      (if (typep (failed self) 'dissect:environment)
          (progn
            (print-failed-env (failed self) (+ report:*indent-amount* indent))
            (format t "~%"))
          (format t "~V@a ~a does not ~a ~a~%" (+ report:*indent-amount* indent) "-" (lhs self) predicate (rhs self))))))

