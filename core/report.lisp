(defpackage :expect/report
  (:use :cl)
  (:import-from :blackbird)
  (:import-from :decorator))

(in-package :expect/report)


(defclass result () ())


(defclass report ()
  ((failures :initform #() :type (vector result))))
