(defpackage :expect/report/dump
  (:use :cl)
  (:import-from :dissect)
  (:import-from :expect/report/report *indent-amount*)
  (:export #:print-failed-env))

(in-package :expect/report/dump)


(defun print-failed-env (failed indent)
  (let ((condition (dissect:environment-condition failed )))
    (format t "~V@a Unexpected ~a: ~a~%" indent "-" (type-of condition) (clean-string condition))
    (dump-stack (dissect:environment-stack failed) (+ 2 indent))))


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


(defun destructure-call (call)
  (with-accessors ((symbol dissect:call) (line dissect:line) (args dissect:args) (file dissect:file)) call
    (let* ((form (format nil "(~a ~{~s~^ ~})" symbol args))
           (package (and (symbolp symbol) (package-name (symbol-package symbol))))
           (fname (and file (file-namestring file)))
           (detail (if fname
                       (format nil "[~a(~a:~a)]" package fname line)
                       (format nil "[~a]" package ))))
      (list form detail))))


(defun dump-stack (stack indent)
  (let* ((stack (butlast (cdr stack)))
         (reduced-stack (remove-if #'(lambda (call) (typep (dissect:call call) 'string)) stack))
         (start
           (or #+sbcl (position-if #'(lambda (call) (eql 'sb-kernel:internal-error (dissect:call call))) reduced-stack)
               0))
         (end (position-if #'(lambda (call) (eql 'eval (dissect:call call))) reduced-stack)))
    (print-stack (subseq reduced-stack (+ start 1) (and end (- end 1))) indent)))


(defun print-stack (stack indent)
  (let* ((calls (mapcar #'destructure-call stack))
         (max-len (max-member-len calls))
         (calls-with-len (mapcar (lambda (call) (cons max-len call)) calls))
         (calls-with-len-ident (mapcar (lambda (call) (cons indent (cons "-" call))) calls-with-len)))
    (format t "~:{~&~V@a ~V<~A~;~> ~8@A~}~%" calls-with-len-ident)))


(defun max-member-len (calls)
  (loop for call in calls maximize (length (car call))))
