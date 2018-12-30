(defpackage :expect/suite.test 
  (:use :cl :expect/suite))

(in-package :expect/suite.test)


(expect:deftest-of suite-of ()
  "Create a suite if it does not exist"
  (typep (suite-of "Some package") 'suite))
