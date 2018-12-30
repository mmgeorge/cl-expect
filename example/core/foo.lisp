(defpackage :example/foo
  (:use :cl)
  (:export #:add #:silly-incr))

(in-package :example/foo)


(defun add (a b)
  (+ a b))


(defun check-value-is-1 (a)
  (unless (eq a 1)
    (error "Got a value that is not 1!"))
    a)


(defun silly-incr (a b)
  "Adds B (which must be 1) to A"
  (add a (check-value-is-1 b)))
