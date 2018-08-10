(defpackage :expect/test
  (:use :cl)
  (:import-from :expect/expect #:expect)
  (:export #:make-test #:name #:body #:add #:expects #:run)
  (:local-nicknames (:expect :expect/expect)))

(in-package :expect/test)


(defstruct result
  failed)


(defclass test ()
  ((name :reader name :initarg :name)
   ;;(body :reader body :initarg :body)
   (expects :reader expects :initform (make-array 0 :fill-pointer t :adjustable t))))


(defun make-test (name)
  (make-instance 'test :name name))


(defun add (self expect)
  (vector-push-extend expect (expects self)))


(defun run (self)
  (loop for expect across (expects self)
        do (safe-eval-expect expect)))



(defun eval-expect (expect)
  (let ((predicate (expect:predicate expect))
        (form (eval (expect:form expect)))
        (expected (eval (expect:expected expect))))
    (if (funcall predicate form expected)
        (make-result :failed nil)
        (make-result :failed t))))

;; safe recursive eval or that thing in the book that lets you trace
;; be able to print exact line that causes conditions?

(defun safe-eval-expect (expect)
  (handler-case (eval-expect expect)
    (t (e)
      (make-result :failed e))))


