#-asdf3.3 (error "Building requires asdf 3.3 but ~a is installed" (asdf:asdf-version))


(asdf:defsystem "expect"
  :class :package-inferred-system
  :pathname "core"
  :depends-on (:expect/exports)
  )


;; (asdf:defsystem "expect/test"
;;   :class :package-inferred-system
;;   :pathname "core"
;;   :depends-on ()
;;   :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))


;;(asdf:register-system-packages :rove '(:rove))
;;(asdf:register-system-packages :blackbird '(:blackbird))
(asdf:register-system-packages :decorator '(:decorator))
