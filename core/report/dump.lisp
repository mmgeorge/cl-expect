(defpackage :expect/report/dump
  (:use :cl :alexandria)
  (:import-from :dissect)
  (:import-from :closer-mop)
  (:import-from :cl-ppcre)
  (:import-from :expect/report/report *indent-amount*)
  (:import-from :expect/util #:get-current-system)
  (:export #:print-failed-env))

(in-package :expect/report/dump)


(defun print-failed-env (failed indent)
  (let ((condition (dissect:environment-condition failed )))
    (print-stack (dissect:environment-stack failed) condition indent)))


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


(defun indent-newlines (str indent)
  (cl-ppcre:regex-replace-all "(\\n)" str (format nil "~%~V@T" indent)))


(defun pretty-print-failed-form (form bad-expr condition indent)
  (let* ((expr-str (format nil "~a" bad-expr))
         (expr-start (search expr-str form))
         (line-start (1+ (position #\newline form :end expr-start :from-end t)))
         (line-end (position  #\newline form :start expr-start))
         (expr-line-start (+ indent (- expr-start line-start)))
         (out (concatenate 'string
                           (format nil "~V@T" indent)
                           (indent-newlines (subseq form 0 line-end) indent)
                           (format nil "~%~V@T~V~~%" expr-line-start (length expr-str) )
                           (format nil "~V@T~a" expr-line-start
                                   (indent-newlines (trim-error-message condition) expr-line-start))
                           ;;(subseq form line-end (length form))
                           )))
    (format t "~A~%" out)))


(defun pretty-print-pathname (pathname system-pathname)
  (when pathname 
    (let ((namestring (namestring pathname))
          (system-namestring (namestring system-pathname)))
      (subseq namestring (length system-namestring) (length namestring)))))


(defun trim-error-message (condition)
  "Trim reference page lookup from error message"
  (let ((condition-text (format nil "~a" condition)))
    (subseq condition-text 0 (search "See" condition-text))))


(defun print-stack (stack condition indent)
  (let* ((current-system (get-current-system))
         (system-name (asdf:component-name current-system))
         (system-pathname (asdf:component-pathname current-system)))
    (flet ((current-system-package-p (platform-call)
             (let* ((other (dissect:call platform-call))
                    (other-prefix
                      (and
                       (symbolp other)
                       (>= (length (package-name (symbol-package other))) (length system-name))
                       (subseq (package-name (symbol-package other)) 0 (length system-name)))))
               (string-equal system-name other-prefix))))
      (let ((pos (position-if #'current-system-package-p stack)))
        (if pos
            ;; Attempt to print a pretty error that includes where the error occured
            (let* ((call (nth pos stack))
                   (prior (and (> pos 0) (nth (1- pos) stack)))
                   (after (nth (1+ pos) stack))
                   (form (dissect:form call)))
              (format t "~V@T~@[In ~a:~]~@[~a, ~]~a:~2%"
                      indent (pretty-print-pathname (dissect:file call) system-pathname)
                      (dissect:line call)
                      (type-of condition))
              (when (and form prior)
                (let ((bad-expr (find-matching-expr (read-from-string form) (extract-symbol-names prior))))
                  (pretty-print-failed-form form bad-expr condition indent)))
              (when after
                (format t "~V@TCalled by ~a~@[:~a~]:~%~V@T~a~2%"
                        indent
                        (and (dissect:file after)
                             (pretty-print-pathname (dissect:file after) system-pathname))
                        (dissect:line after)
                        (+ 2 indent)
                        (dissect:call after))))
            ;; Otherwise just dump the error
            (format t "~V@T~a~%" indent condition))))))


(defun max-member-len (calls)
  (loop for call in calls maximize (length (car call))))
