(defpackage :expect/expect.test 
  (:use :cl :expect/expect)
  (:import-from :expect/report/report #:failed))

(in-package :expect/expect.test)


(expect:deftest-of make-expect ()
  "Bad expect generates failed report"
  (let* ((predicate 'eq)
         (form '1)
         (expected '2)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (eq (failed (safe-eval instance)) t))))


(expect:deftest-of make-expect ()
  "Good expect generates succesful report"
  (let* ((predicate 'eq)
         (form '1)
         (expected '1)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (eq (failed (safe-eval instance)) nil))))
