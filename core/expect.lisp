(defpackage :expect/expect
  (:use :cl)
  (:import-from :expect/report/expect)
  (:export #:expect #:make-expect
           #:predicate #:form #:expected
           #:safe-eval)
  (:local-nicknames (:report/expect :expect/report/expect)))

(in-package :expect/expect)


(defclass expect ()
  ((predicate :reader predicate :initarg :predicate)
   (form :reader form :initarg :form)
   (expected :reader expected :initarg :expected)))


(defun make-expect (predicate form expected)
  (make-instance 'expect :predicate predicate :form form :expected expected))


(defmethod make-load-form ((self expect) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of self)
                  :predicate ',(predicate self)
                  :form ',(form self)
                  :expected ',(expected self)))


(defun safe-eval (expect)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((enviornment nil))
    (restart-case
        (dissect:with-truncated-stack ()
      (handler-bind ((error
                       #'(lambda (e)
                           (setf enviornment (dissect:capture-environment e))
                           (invoke-restart (find-restart 'dump-stack)))))
        (eval-expect expect)))
      (dump-stack ()
        (report/expect:make-expect enviornment (predicate expect) (form expect) (expected expect) nil nil)))))


(defun eval-expect (expect)
  (let ((predicate (predicate expect))
        (form (eval (form expect)))
        (expected (eval (expected expect))))
    (if (funcall predicate form expected)
        (report/expect:make-expect nil (predicate expect) (form expect) (expected expect) form expected)
        (report/expect:make-expect t (predicate expect) (form expect) (expected expect) form expected))))

