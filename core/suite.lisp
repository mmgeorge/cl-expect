(defpackage :expect/suite
  (:use :cl :blackbird :alexandria)
  (:import-from :expect/test)
  (:import-from :expect/report/report #:record)
  (:import-from :expect/report/suite)
  (:export #:suite #:suite-of #:suite-exists-p #:register #:clear #:clear-all #:tests #:run #:load-timestamp)
  (:local-nicknames (:test :expect/test)
                    (:report :expect/report/report)
                    (:report/suite :expect/report/suite)))

(in-package :expect/suite)


(defvar *suites* (make-hash-table :test 'equal))


(defclass suite ()
  ((package-of :accessor package-of :initarg :package)
   (tests :accessor tests :initform (make-hash-table :test 'equal)  :type 'hash-table)
   (test-names :accessor test-names :initform nil  :type 'list)
   (load-timestamp :accessor load-timestamp :initform nil)))


(defun make-suite (package)
  (make-instance 'suite :package package))


(defun register (self test)
  (let ((tests (gethash (test:name test) (tests self))))
    (setf (gethash (test:name test) (tests self))
          (if tests
              (insert-or-replace-test test tests)
              (progn
                ;; Add to ordered list of test-names
                (setf (test-names self) (nconc (test-names self) (list (test:name test))))
                (list test))))))
    
    
(defun insert-or-replace-test (test list-of-tests)
  (flet ((same-test-p (target)
           (and (string-equal (test:suite-name target) (test:suite-name test))
                (string-equal (test:name target) (test:name test))
                (string-equal (test:description target) (test:description test)))))
    (let ((pos (position-if #'same-test-p list-of-tests)))
      (if pos
          (setf (nth pos list-of-tests) test)
          (nconc list-of-tests (list test)))
      list-of-tests)))


(defun clear (self)
  (setf (test-names self) nil)
  (setf (tests self) (make-hash-table :test 'equal)))


(defun clear-all ()
  (setf *suites* (make-hash-table :test 'equal)))


(defun name (package)
  (let* ((package-name (string-downcase (if (typep package 'string) package (package-name package))))
         (len (length package-name)))
    (if (and (> len 5) (string-equal (subseq package-name (- len 5) len) ".test"))
        (subseq package-name 0 (- len 5))
        package-name)))


(defun suite-exists-p (package)
  (let ((name (name package)))
    (gethash name *suites*)))


(defun suite-of (package)
  (let  ((name (name package)))
    (or (gethash name *suites*)
        (setf (gethash name *suites*)
              (make-suite name)))))


(defun amap-wait (function list)
  "Run map over a list of promises, finishing the returned promises once all values 
have been fullfilled. Unlike amap, amap-wait waits on each individual promise before 
executing the next in the sequence, ensuring promise is resolved in turn."
  (amap #'identity
          (reduce #'(lambda (promise-list curr)
                      (cons
                       (wait (car promise-list)
                         (funcall function curr))
                     promise-list)) list :initial-value nil)))


(defun run (self)
  (let ((report (report/suite:make-suite (hash-table-count (tests self)) (package-of self)))
        (suite-name (package-of self)))
    (labels ((run-tests (test-name)
               (alet* ((function-tests (gethash test-name (tests self)))
                       (test-count (length function-tests))
                       (test-reports (amap-wait #'test:run function-tests))
                       (failed-test-count (reduce #'record-test test-reports :initial-value 0))
                       (passed-test-count (- test-count failed-test-count))
                       (result (if (eq passed-test-count test-count) "PASS" "FAIL")))
                 (format t "[~a] ~a:~a [~a/~a]~%" result suite-name test-name passed-test-count test-count)))
             (record-test (failed-test-count test-report)
               (record report test-report)
               (if (> (report:nested-failed-length test-report) 0)
                   1
                   failed-test-count)))
      (let* ((test-names (test-names self)))
        (wait (amap-wait #'run-tests test-names)
          (format t "~%")
          report)))))
