;;;; package.lisp

(defpackage #:cl-junit-xml
  (:use #:cl #:iterate)
  (:export #:write-xml #:make-junit #:make-testcase #:make-testsuite #:add-child))
