(defpackage :expect/test
  (:use :cl :alexandria :blackbird)
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
   (expects :reader expects :initform (make-array 0 :fill-pointer t :adjustable t))
   (body :accessor body :initarg :body)))


(defun make-test (name description suite-name)
  (make-instance 'test :name (string-downcase name)
                       :description description
                       :suite-name (string-downcase suite-name)))


(defun add (self expect)
  (vector-push-extend expect (expects self)))


(defun clear (self)
  (setf (slot-value self 'expects) (make-array 0 :fill-pointer t :adjustable t)))


(defun run (self)
  (with-accessors ((suite-name suite-name) (name name) (description description) (body body)) self
    ;; We may encounter an error when evaluating the test-body. In this case, we create a
    ;; new test-report that includes details about the failure
    (clear self)
    (attach
     ;; safe-call may evaluate to a promise if the test-body returns one
     (safe-call self)
     (lambda (maybe-result)
       (if (typep maybe-result 'report/test:test)
           maybe-result
           (let ((report (report/test:make-test (name self) (suite-name self) (description self))))
             (loop for expect across (expects self)
                   for result = (expect:safe-eval expect)
                   do
                      (when (report:failed result)
                        (setf (report:failed report) t))
                      (record report result))
             report))))))


(defun safe-call (self)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((enviornment nil))
    (flet ((handle-error (e)
             (setf enviornment (dissect:capture-environment e))
             (invoke-restart (find-restart 'capture))))
      (restart-case (dissect:with-truncated-stack ()
                      (handler-bind ((error #'handle-error))
                        ;; User may return a promise
                        (catcher
                         (funcall (body self))
                         ;; Todo - improve promise handling. Any way we can dump some kind of a stack trace here?
                         ;; passing an error as enviornment will log its output instead of a trace
                         (t (e)
                            (setf enviornment e)
                            (invoke-restart (find-restart 'capture))))))
        (capture ()
          (let ((report (report/test:make-test (name self) (suite-name self) (description self) enviornment)))
            (setf (report:failed report) t)
            report))))))
