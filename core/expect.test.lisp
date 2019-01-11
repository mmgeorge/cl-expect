(defpackage :expect/expect.test 
  (:use :cl :expect/expect)
  (:import-from :expect/report/report #:failed)
  (:import-from :dissect))

(in-package :expect/expect.test)


(expect:deftest-of make-expect ()
  "Good expect generates succesful report"
  (let* ((predicate 'eq)
         (form '1)
         (expected '1)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (eq (failed (safe-eval instance)) nil))))


(expect:deftest-of make-expect ()
  "Bad expect generates failed report"
  (let* ((predicate 'eq)
         (form '1)
         (expected '2)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (eq (failed (safe-eval instance)) t))))


(expect:deftest-of make-expect ()
  "Some value"
  (bb:with-promise (resolve reject)
    (error "bad news"))
  )


(expect:deftest-of make-expect ()
  "Gibberish expect generates a failed report with a stack trace"
  (let* ((predicate 'NONSENSE)
         (form 'SOME_SYMBOL?)
         (expected '1)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (typep (failed (safe-eval instance)) 'dissect:environment))))


(expect:deftest-of make-expect ()
  "Expect an error"
  (let* ((predicate 'typep)
         (form '(error "Some error condition"))
         (expected ''simple-error)
         (uneval `(,predicate ,form ,expected))
         (instance (make-expect uneval predicate form expected)))
    (expect:expect (eq (failed (safe-eval instance)) nil))))
