(defpackage :expect/report/output/junit
  (:use :cl)
  (:import-from :expect/report/report #:report)
  (:import-from :expect/report/test #:test)
  (:import-from :expect/report/suite #:suite)
  (:import-from :expect/report/expect #:expect)
  (:import-from :cxml)
  (:export #:junit #:make)
  (:local-nicknames
   (:report :expect/report/report)
   (:test :expect/report/test)
   (:suite :expect/report/suite)))

(in-package :expect/report/output/junit)


(defclass junit ()
  ((path :reader path :initarg :path)))


(defun make (path)
  (make-instance 'junit :path path))


(defmethod report:write-to-output ((report report) (target junit))
  (loop for child in (report:children report) do
    (report:write-to-output child target)))


(defmethod report:write-to-output ((suite suite) (target junit))
  (let* ((name (substitute #\. #\/ (suite:suite-name suite)))
         (directory (append (pathname-directory (path target)) (list name)))
         (path (make-pathname :directory directory :name "results" :type "xml")))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 2 :canonical nil)
        (cxml:with-element "testsuites"
          (cxml:with-element "testsuite"
            (cxml:attribute "name" (suite:suite-name suite))
            (cxml:attribute "errors" 0)
            (cxml:attribute "tests" 0)
            ;;(cxml:attribute "time" 0)
            ;;(cxml:attribute "timestamp" 0)
            (loop for test in (report:children suite) do
              (report:write-to-output test target))))))))
  

  (defmethod report:write-to-output ((test test) (target junit))
    (cxml:with-element "testcase"
      (cxml:attribute "classname" (test:name test))
      (cxml:attribute "name" (test:description test))
      (cxml:with-element "failure"
        (cxml:attribute "message" "It broke")
        (cxml:text "Some random text failure"))
      ))
