(defpackage :expect/test
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/expect #:expect)
  (:import-from :expect/report/report #:record #:children)
  (:import-from :expect/report/report)
  (:import-from :expect/report/test)
  (:export #:make-test #:name #:description #:suite-name #:body #:add #:expects #:run #:print-result)
  (:local-nicknames (:expect :expect/expect)
                    (:report :expect/report/report)
                    (:report/test :expect/report/test)))

(in-package :expect/test)


(defclass test ()
  ((name :reader name :initarg :name)
   (description :reader description :initarg :description)
   (suite-name :reader suite-name :initarg :suite-name)
   (expects :reader expects :initform (make-array 0 :fill-pointer t :adjustable t))))


(defun make-test (name description suite-name)
  (make-instance 'test :name (string-downcase name)
                       :description description
                       :suite-name (string-downcase suite-name)))



(defun add (self expect)
  (vector-push-extend expect (expects self)))


(defun run (self)
  (with-accessors ((suite-name suite-name) (name name) (description description)) self
    (let ((report (report/test:make-test (name self) (suite-name self) (description self))))
      (loop for expect across (expects self)
            for result = (expect:safe-eval expect)
            do (record report result))
      report)))
      

