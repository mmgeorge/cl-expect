(uiop/package:define-package :expect/exports
    (:use :cl)
  (:nicknames :expect)
  (:use-reexport #:expect/macros #:expect/runner))
