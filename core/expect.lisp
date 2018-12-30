(defpackage :expect/expect
  (:use :cl)
  (:import-from :blackbird)
  (:import-from :expect/report/expect)
  (:export #:expect #:make-expect
           #:predicate #:form #:expected
           #:safe-eval)
  (:local-nicknames (:report/expect :expect/report/expect)))

(in-package :expect/expect)


(defclass expect ()
  ((uneval :reader uneval :initarg :uneval)
   (predicate :reader predicate :initarg :predicate)
   (form :reader form :initarg :form)
   (expected :reader expected :initarg :expected)))


(defun make-expect (uneval predicate form expected)
  (make-instance 'expect :uneval uneval :predicate predicate :form form :expected expected))


(defmethod make-load-form ((self expect) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of self)
                  :predicate ',(predicate self)
                  :form ',(form self)
                  :expected ',(expected self)))


(defun safe-condition-check (predicate expected condition)
  "Check whether the given error matches the expected type"
  ;; TODO: Add better handling for when user checks for an error using the wrong type.
  ;; For instance, TYPE-ERROR is expected but SIMPLE-ERROR is thrown. In this case, a cryptic
  ;; failure will be displayed
  (handler-case
      (if (eq predicate 'typep)
          (typep condition (eval expected))
          (funcall predicate
                   (class-of condition)
                   (class-of (eval expected))))
    (t () nil)))


(defun safe-eval (expect)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((enviornment nil))
    (flet ((handle-error (e)
             (if (safe-condition-check (predicate expect) (expected expect) e)
                 ;; TODO: tidy up the logic here 
                 ;; Because we evaluate this for form, pred, and expected, an error in
                 ;; evaluating one of these components (other than form) could lead to a false positive
                 (invoke-restart (find-restart 'condition-pred-holds))
                 (progn
                   (setf enviornment (dissect:capture-environment e))
                   (invoke-restart (find-restart 'capture))))))
    (restart-case (dissect:with-truncated-stack ()
                    (handler-bind ((error #'handle-error))
                      (eval-expect expect)))
      (condition-pred-holds ()
        (report/expect:make-expect nil (uneval expect) (predicate expect) (form expect)
                                   (expected expect) nil nil))
      (capture ()
        (report/expect:make-expect enviornment (uneval expect) (predicate expect) (form expect) (expected expect)
                                   nil nil))))))


(defun eval-expect (expect)
  (let ((predicate (predicate expect))
        (form (eval (form expect)))
        (expected (handler-case (eval (expected expect)) (t () (expected expect)))))
    ;; If we have a blackbird promise, loop until it is resolved.
    ;; TODO: add better promise handling, e.g. defer test results
    (when (typep form 'blackbird:promise)
      (loop while (not (blackbird:promise-finished-p form)) do (sleep 0.250))
      (blackbird:attach form (lambda (result) (setf form result))))
    (if (funcall predicate form expected)
        (report/expect:make-expect nil (uneval expect) (predicate expect) (form expect) (expected expect) form expected)
        (report/expect:make-expect t (uneval expect) (predicate expect) (form expect) (expected expect) form expected))))

