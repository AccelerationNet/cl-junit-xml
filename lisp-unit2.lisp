(in-package #:cl-junit-xml)

(defmethod write-xml ((results lisp-unit2:test-results-db) sink &key pretty-p)
  (let* ((junit (make-junit))
         (suite (add-child junit
                           (make-testsuite (lisp-unit2::name results)))))
    (flet ((summary (test-result)
             (with-output-to-string (lisp-unit2:*test-stream*)
               (lisp-unit2:print-summary test-result))))

      (iter (for test-result in-sequence (lisp-unit2:results results))
      (add-child suite
                 (make-testcase
                  (lisp-unit2::name (lisp-unit2:unit-test test-result))
                  (name suite)
                  (- (lisp-unit2::end-time test-result)
                     (lisp-unit2::start-time test-result))
                  :error (when (lisp-unit2:errors test-result)
                           (summary test-result))
                  :failure (when (lisp-unit2:failed test-result)
                             (summary test-result))))))

    (write-xml junit sink :pretty-p pretty-p)))

(defmethod write-xml ((c lisp-unit2:all-tests-complete) sink &key pretty-p)
  (write-xml (lisp-unit2:results c) sink :pretty-p pretty-p))