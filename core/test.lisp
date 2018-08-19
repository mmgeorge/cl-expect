(defpackage :expect/test
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/expect #:expect)
  (:export #:make-test #:name #:body #:add #:expects #:run
           #:print-result)
  (:local-nicknames (:expect :expect/expect)))

(in-package :expect/test)


(defstruct result
  failed
  expect
  lhs
  rhs)


(defclass test ()
  ((name :reader name :initarg :name)
   (suite-name :reader suite-name :initarg :suite-name)
   ;;(body :reader body :initarg :body)
   (expects :reader expects :initform (make-array 0 :fill-pointer t :adjustable t))))


(defun make-test (name suite-name)
  (make-instance 'test :name name :suite-name suite-name))


(defun add (self expect)
  (vector-push-extend expect (expects self)))


(defun run (self)
  (let ((failures
          (loop for expect across (expects self)
                for result = (safe-eval-expect expect)
                if (result-failed result)
                  collect result)))
    (print-summary self failures)
    failures))


(defun print-summary (self failures)
  (let ((count (length (expects self))))
    (if failures
        (format t "[FAIL] ~a:~a [~a/~a]~%" (suite-name self) (name self) (- count (length failures)) count)
        (format t "[PASS] ~a:~a [~a/~a]~%" (suite-name self) (name self) count count)
        )))


(defun print-result (result)
  (when (result-failed result)
    (with-accessors ((predicate expect:predicate) (form expect:form) (expected expect:expected))
        (result-expect result)
      (format t "(~a ~a ~a)~%" predicate form expected)
      (if (typep (result-failed result) 'dissect:environment)
          (print-failed-env (result-failed result))
          (format t "   -  ~a does not ~a ~a~%" (result-lhs result) predicate (result-rhs result))
      ))))


(defun clean-string (str)
  "Remove newlines from error message"
  (let ((str (substitute #\space #\newline (format nil "~a" str))))
    (with-output-to-string (s)
      (write-char (aref str 0) s) 
      (loop for i from 1 below (length str)
            for prev = (aref str (- i 1))
            for curr = (aref str i)
            when (not (and (eql #\space prev)
                           (eql #\space curr)))
              do (write-char curr s)))))
  

(defun print-failed-env (failed)
  (let ((condition (dissect:environment-condition failed )))
    (format t "   -  Unexpected ~a: ~a~%" (type-of condition) (clean-string condition))
    (dump-stack (dissect:environment-stack failed))
  ))

(defun eval-expect (expect)
  (let ((predicate (expect:predicate expect))
        (form (eval (expect:form expect)))
        (expected (eval (expect:expected expect))))
    (if (funcall predicate form expected)
        (make-result :failed nil :expect expect :lhs form :rhs expected)
        (make-result :failed t :expect expect :lhs form :rhs expected))))


;; (defun print-call (call)
;;   (let ((*print-case* :downcase))
;;   (with-accessors ((text dissect:call) (line dissect:line) (args dissect:args) (file dissect:file)) call
;;     (format t "~a"  (cons text args))
;;     (if file
;;         (format t " [~a(~a:~a)]~%" (package-name (symbol-package text)) (file-namestring file) line)
;;         (format t " [~a]~%" (package-name (symbol-package text)))))))


(defun destructure-call (call)
  (with-accessors ((symbol dissect:call) (line dissect:line) (args dissect:args) (file dissect:file)) call
    (let* ((form (format nil "(~a ~{~s~^ ~})" symbol args))
           (package (package-name (symbol-package symbol)))
           (fname (and file (file-namestring file)))
           (detail (if fname
                       (format nil "[~a(~a:~a)]" package fname line)
                       (format nil "[~a]" package ))))
      (list form detail))))


(defun max-member-len (calls)
  (loop for call in calls maximize (length (car call))))


(defun print-stack (stack)
    (let* ((calls (mapcar #'destructure-call stack))
           (max-len (max-member-len calls))
           (calls-with-len (mapcar (lambda (call) (cons max-len call)) calls)))
      (format t "~:{~&      - ~V<~A~;~> ~8@A~}" calls-with-len)))



(defun dump-stack (stack)
  (let* ((stack (butlast (cdr stack)))
         (reduced-stack (remove-if #'(lambda (call) (typep (dissect:call call) 'string)) stack))
         (start
           (or #+sbcl (position-if #'(lambda (call) (eql 'sb-kernel:internal-error (dissect:call call))) reduced-stack)
               0))
         (end (position-if #'(lambda (call) (eql 'eval (dissect:call call))) reduced-stack)))
    (print-stack (subseq reduced-stack (+ start 1) (- end 1)))))

  
(defun safe-eval-expect (expect)
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
        (make-result :failed enviornment :expect expect)))))


(defun 3funk (a b)
  (+ a b))

(defun 2funk (a b)
   (3funk a b))

(defun myfunkysadfasdfasdsdfasdfasdf (a b)
  (2funk a b))

(defun getfirst (a) (car a))

;(myfunky 1 (list "2"))


    




