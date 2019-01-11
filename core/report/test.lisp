(defpackage :expect/report/test
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/report/report #:failed)
  (:import-from :expect/report/dump #:print-failed-env)
  (:export #:test #:make-test)
  (:local-nicknames (:report :expect/report/report)))

(in-package :expect/report/test)


(defclass test (report:report)
  ((name :reader name :initarg :name)
   (description :reader description :initarg :description)
   (suite-name :reader suite-name :initarg :suite-name)
   (failed-env :reader failed-env :initarg :failed-env)))


(defun make-test (name suite-name description &optional (failed-env nil))
  "Report the result of a test. FAILED-ENV can be nil, a dissect:enviornment, or an error"
  (make-instance 'test :name name :suite-name suite-name :description description :failed-env failed-env))


(defmethod report:print-report ((self test) indent)
  (cond ((failed-env self)
         (format t "~%")
         (format t "~Va ~a:~a [~a] encountered an unexpected error:~%"
                 indent "Test of" (suite-name self) (name self) (description self))
         (if (typep (failed-env self) 'dissect:environment )
             (print-failed-env (failed-env self) (+ report:*indent-amount* indent))
             (format t "~Va ~a~%" indent "" (failed-env self))))
        ((find-if #'failed (report:children self))
         (format t "~%")
         (format t "~Va ~a:~a [~a] failed [~a] expects:~%"
                 indent "Test of" (suite-name self) (name self) (description self) (report:nested-failed-length self))
         (call-next-method))))
