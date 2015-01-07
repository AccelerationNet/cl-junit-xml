(in-package #:cl-junit-xml)

(defmethod write-xml ((results lisp-unit::test-results-db) sink &key pretty-p name &allow-other-keys)
  (let* ((junit (make-junit))
         (suite (add-child junit (make-testsuite "lisp-unit"))))
    (maphash #'(lambda (test-name test-result)
                 (add-child
                  suite
                  (make-testcase test-name (or name (name suite))
                                 (/ (lisp-unit::run-time test-result)
                                    internal-time-units-per-second)
                                 :error (with-output-to-string (s)
                                          (lisp-unit:print-errors test-result s))
                                 :failure (with-output-to-string (s)
                                            (lisp-unit:print-failures test-result s)))))
             (lisp-unit::database results))
    (write-xml junit sink :pretty-p pretty-p)))

(defmethod write-xml ((c lisp-unit:test-run-complete) sink &key pretty-p name &allow-other-keys)
  (write-xml (lisp-unit:results c) sink :pretty-p pretty-p :name name))
