(defpackage :expect/fixture
  (:use :cl)
  (:export #:fixture #:before #:after))

(in-package :expect/fixture)


(defclass fixture ()
  ((before :reader before :initarg :before)
   (after :reader after :initarg :after)))
