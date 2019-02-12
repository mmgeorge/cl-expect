(defpackage :expect/macros
  (:use :cl)
  (:import-from :bordeaux-threads)
  (:import-from :expect/suite)
  (:import-from :expect/expect #:make-expect)
  (:import-from :expect/fixture)
  (:export #:deftest-of #:expect #:defixture #:with-cleanup #:capture-error #:*print-compile-message*)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)
                    (:fixture :expect/fixture)))

(in-package :expect/macros)


(defvar *cl-expect-test* nil)
(defvar *in-fixture* nil)
(defvar *print-compile-message* t)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-fixture-list (fixture-list body)
    (if fixture-list
      (destructuring-bind (var fixture) (car fixture-list)
        (let* ((fixture (eval fixture))
               (before (fixture:before fixture))
               (after (fixture:after fixture))
               (next (expand-fixture-list (cdr fixture-list) body))
               (inner-form (if after `(bb:finally ,next (funcall #',after ,var)) next)))
          `(bb:attach ,before (lambda (,var) ,inner-form))))
      `(progn ,@body))))


;; (bb:attach
;;  (fixture:run before)
;;  (lambda (server)
;;    (bb:finally 
;;        (wait (client:send-await server "hi folks!~%" )
;;          (expect (string-equal (client:read self) "hi folks~%"))))
;;    (funcall #'fixture:after server ))))



(defmacro deftest-of (function fixtures &body body)
  (let ((desc (car body))
        (test-body (cdr body)))
    (unless (typep desc 'string)
      (error "Malformed deftest-of form. Expected to find test description but found ~a" (type-of desc)))
  `(progn
     (let ((test (test:make-test ',function ,desc (package-name (symbol-package ',function)))))
       (setf (test:body test )
             (lambda ()
               (let* ((*cl-expect-test* test)
                      ;; Add test to list of bordeaux threads default bindings which will cause
                      ;; it to get passed to newly created threads
                      (bt:*default-special-bindings*
                        (acons '*cl-expect-test* test bt:*default-special-bindings*)))
                 ,(expand-fixture-list fixtures test-body))))
       (when *print-compile-message*
         (format t "Adding test definition for ~a~%" (test:name test)))
       (suite:register (suite:suite-of *package*) test))
       *cl-expect-test*)))


(defmacro expect ((predicate form expected))
  (let ((uneval `(,predicate ,form ,expected)))
    `(progn 
       (unless *cl-expect-test*
         (error "Expect must be called within a test!"))
       (test:add *cl-expect-test* (make-expect ,uneval ',predicate ,form ,expected)))))


(defmacro capture-error (error-type &body body)
  `(handler-case ,@body
     (,error-type (e) e)))


(defmacro defixture (name () &body body)
  `(let ((*in-fixture* t))
     ,(if (eq (caar body) 'with-cleanup)
          `(defparameter ,name ,@(macroexpand body))
          `(defparameter ,name  
             (make-instance 'fixture:fixture :before '(progn ,@body) :after nil)))))


(defmacro with-cleanup (before cleanup)
  `(progn
     (unless *in-fixture*
       (error "with-cleanup must appear within a defixture form"))
     (make-instance 'fixture:fixture :before `(progn ,',before) :after ',cleanup)))
