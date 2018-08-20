(defpackage :expect/test
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/expect #:expect)
  (:import-from :expect/report/report #:record #:children)
  (:import-from :expect/report/report)
  (:import-from :expect/report/test)
  (:export #:make-test #:name #:body #:add #:expects #:run #:print-result #:suite-name)
  (:local-nicknames (:expect :expect/expect)
                    (:report :expect/report/report)
                    (:report/test :expect/report/test)))

(in-package :expect/test)


(defclass test ()
  ((name :reader name :initarg :name)
   (suite-name :reader suite-name :initarg :suite-name)
   (expects :reader expects :initform (make-array 0 :fill-pointer t :adjustable t))))


(defun make-test (name suite-name)
  (make-instance 'test :name name :suite-name suite-name))


(defun add (self expect)
  (vector-push-extend expect (expects self)))


(defun run (self)
  (let ((report (report/test:make-test (name self) (suite-name self)))
        (count (length (expects self))))
    (loop for expect across (expects self)
          for result = (expect:safe-eval expect)
            do (record report result))
    (if (> (report:nested-failed-length report) 0)
        (format t "[FAIL] ~a:~a [~a/~a]~%" (suite-name self) (name self) (- count (report:nested-failed-length report)) count)
        (format t "[PASS] ~a:~a [~a/~a]~%" (suite-name self) (name self) count count))
    report))

