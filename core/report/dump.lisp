(defpackage :expect/report/dump
  (:use :cl :alexandria)
  (:import-from :dissect)
  (:import-from :closer-mop)
  (:import-from :expect/report/report *indent-amount*)
  (:import-from :expect/util #:get-current-system)
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
  ;(let* ((stack (butlast (cdr stack)))
         ;(reduced-stack (remove-if #'(lambda (call) (typep (dissect:call call) 'string)) stack))
         ;(start
           ;(or #+sbcl (position-if #'(lambda (call) (eql 'sb-kernel:internal-error (dissect:call call))) reduced-stack)
               ;0))
                                        ;(end (position-if #'(lambda (call) (eql 'eval (dissect:call call))) reduced-stack)))
  ;(format t "hi WORLD~%")
  (print-stack stack indent)
  )



(defun find-matching-expr (form target-list)
  (flet ((list-has-symbol (symbol-name)
           (find-if (lambda (target-name) (string-equal symbol-name target-name)) target-list)))
    (loop for expr in form do
      (if (listp expr)
          (let ((result (find-matching-expr expr target-list)))
            (when result
              (return-from find-matching-expr result)))
          (when (list-has-symbol (symbol-name expr))
            (return-from find-matching-expr form))))))


(defun extract-symbol-names (call)
  (flet ((process-arg (arg)
           (cond ((typep arg 'standard-generic-function) (closer-mop:generic-function-name arg))
                 ((symbolp arg) (symbol-name arg))
                 (t "[unknown]"))))
    (let ((call-name (symbol-name (dissect:call call)))
          (args (dissect:args call)))
      (cons call-name (mapcar #'process-arg args)))))


(defun pretty-print-failed-form (form bad-expr)
  (let* ((expr-str (format nil "~a" bad-expr))
         (expr-start (search expr-str form))
         (line-start (1+ (position #\newline form :end expr-start :from-end t)))
         (line-end (position  #\newline form :start expr-start))
         (expr-line-start (- expr-start line-start))
         (out (concatenate 'string
                           (subseq form 0 line-end)
                           (format nil "~%~V@T~V~" expr-line-start (length expr-str) )
                           (subseq form line-end (length form)))))
    (format t "~%~A~%" out)))


(defun print-stack (stack indent)
  (let* ((current-system (get-current-system))
         (system-name (asdf:component-name current-system)))
    (flet ((current-system-package-p (platform-call)
             (let* ((other (dissect:call platform-call))
                    (other-prefix
                      (and
                       (symbolp other)
                       (>= (length (package-name (symbol-package other))) (length system-name))
                       (subseq (package-name (symbol-package other)) 0 (length system-name)))))
               (string-equal system-name other-prefix))))
      (let* ((pos (position-if #'current-system-package-p stack))
             (call (nth pos stack))
             (prior (nth (1- pos) stack))
             (form (dissect:form call))
             (bad-expr (find-matching-expr (read-from-string form) (extract-symbol-names prior))))
        (pretty-print-failed-form form bad-expr)))))
               
  
  ;(loop for call in stack
        ;do (describe call))); (format t "  - ~a~%" (dissect:call call))))
  
  ;(let* ((calls (mapcar #'destructure-call stack))
   ;      (max-len (max-member-len calls))
    ;     (calls-with-len (mapcar (lambda (call) (cons max-len call)) calls))
     ;    (calls-with-len-ident (mapcar (lambda (call) (cons indent (cons "-" call))) calls-with-len)))
    ;(format t "~:{~&~V@a ~V<~A~;~> ~8@A~}~%" calls-with-len-ident)))


(defun max-member-len (calls)
  (loop for call in calls maximize (length (car call))))
