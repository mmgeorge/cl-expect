(defpackage :expect/report/expect
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/report/report #:failed)
  (:import-from :expect/report/dump #:write-failed-env)
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


(defmethod report:print-report ((self expect) stream indent)
  (when (failed self)
    (with-accessors ((uneval uneval) (predicate predicate) (form form) (expected expected)) self
      ;;(format t "~V@a ~a -> (~s ~s ~s)~%" indent "-" uneval predicate form expected)
      (format t "~V@T~a (~s ~s ~s)~%" indent "-" predicate form expected)
      (if (typep (failed self) 'dissect:environment)
          (progn
            (write-failed-env (failed self) stream (+ report:*indent-amount* indent))
            (format stream "~%"))
          (format stream "~V@T~a ~a does not ~a ~a~2%"
                  (+ report:*indent-amount* indent) "-" (lhs self) predicate (rhs self))))))

