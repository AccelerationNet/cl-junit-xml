;;;; cl-junit-xml.lisp

(in-package #:cl-junit-xml)

;;; "cl-junit-xml" goes here. Hacks and glory await!

(defgeneric write-xml (junit-xml sink &key pretty-p)
  (:documentation "write the junit to the given sink (string, pathname, T, nil)")
  (:method (junit-xml (sink string) &key pretty-p)
    (write-xml junit-xml (pathname sink) :pretty-p pretty-p))
  (:method (junit-xml (sink T) &key pretty-p)
    (format T (write-xml junit-xml nil :pretty-p pretty-p)))
  (:method (junit-xml (sink pathname) &key pretty-p)
    (with-open-file (stream sink :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (%write-xml junit-xml
                  (cxml:make-octet-stream-sink
                   stream :encoding :utf-8 :indentation (when pretty-p 2))))
    sink)
  (:method (junit-xml (sink null) &key pretty-p)
    (with-output-to-string (s)
      (%write-xml junit-xml
                  (cxml:make-character-stream-sink
                   s :encoding :utf-8 :indentation (when pretty-p 2))))))

(defun %write-xml (junit-xml sink)
  (cxml:with-xml-output sink
    (cxml:with-element "testsuites"
      (iter
        (for id from 0)
        (for suite in (testsuites junit-xml))
<<<<<<< HEAD
        (cxml:with-element "testsuite"
          (cxml:attribute "name" (name suite))
          (cxml:attribute "package" "")
          (cxml:attribute "timestamp" (timestamp suite))
          (cxml:attribute "id" id)
          (cxml:attribute "tests" (length (testcases suite)))
          (cxml:attribute "errors" (count-if #'error-text (testcases suite)))
          (cxml:attribute "failures" (count-if #'failure-text (testcases suite)))
          (cxml:attribute "time" (format nil "~,1f"
                                         (reduce #'+ (testcases suite)
                                                 :key #'duration)))
          (dolist (testcase (testcases suite))
            (cxml:with-element "testcase"
              (cxml:attribute "name" (name testcase))
              (cxml:attribute "classname" (class-name testcase))
              (cxml:attribute "time" (format nil "~,1f"
                                             (duration testcase)))
              (alexandria:when-let ((text (error-text testcase)))
                (cxml:with-element "error" (cxml:text text)))
              (alexandria:when-let ((text (failure-text testcase)))
                (cxml:with-element "failure" (cxml:text text)))))))))
  )

(defun make-junit (&key testsuites)
  (make-instance 'junit-xml :testsuites testsuites))

(defun make-testsuite (name &key testcases timestamp)
  (make-instance 'junit-testsuite
                 :name name :testcases testcases :timestamp timestamp))

(defun make-testcase (name class-name duration &key error failure)
  (make-instance 'junit-testcase :name name :duration duration :class-name class-name
                 :error-text error :failure-text failure))

(defgeneric add-child (parent child)
  (:method ((p junit-xml) (c junit-testsuite))
    (push c (testsuites p))
    c)
  (:method ((p junit-testsuite) (c junit-testcase))
    (push c (testcases p))
    c))

(defclass junit-xml ()
  ((testsuites :accessor testsuites :initarg :testsuites)))

(defclass junit-testsuite ()
  ((testcases :accessor testcases :initarg :testcases)
   (name :reader name :initarg :name)
   (timestamp :reader timestamp :initarg :timestamp)))

(defclass junit-testcase ()
  ((name :reader name :initarg :name)
   (duration :reader duration :initarg :duration)
   (class-name :reader class-name :initarg :class-name)
   (error-text :reader error-text :initarg :error-text)
   (failure-text :reader failure-text :initarg :failure-text)))
