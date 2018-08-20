#-asdf3.3 (error "Building requires asdf 3.3 but ~a is installed" (asdf:asdf-version))


(asdf:defsystem "expect"
  :class :package-inferred-system
  :pathname "core"
  :depends-on (:expect/runner
               :expect/macros
               :expect/example/example
               :expect/example/example2)
  ;;:in-order-to ((asdf:test-op (asdf:test-op "expect/test")))

  )


;; (asdf:defsystem "expect/test"
;;   :class :package-inferred-system
;;   :pathname "core"
;;   :depends-on ()
;;   :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))


;;(asdf:register-system-packages :rove '(:rove))
;;(asdf:register-system-packages :blackbird '(:blackbird))
(asdf:register-system-packages :decorator '(:decorator))
