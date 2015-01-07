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
    (ensure-directories-exist sink)
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
  (with-xml-output sink
    (with-element "testsuites"
      (iter
        (for id from 0)
        (for suite in (testsuites junit-xml))
        (with-element "testsuite"
          (attribute "name" (name suite))
          (attribute "package" "")
          (attribute "timestamp" (timestamp suite))
          (attribute "id" id)
          (attribute "tests" (length (testcases suite)))
          (attribute "errors" (count-if #'error-text (testcases suite)))
          (attribute "failures" (count-if #'failure-text (testcases suite)))
          (attribute "time" (format nil "~,1f"
                                         (reduce #'+ (testcases suite)
                                                 :key #'duration)))
          (dolist (testcase (testcases suite))
            (with-element "testcase"
              (attribute "name" (name testcase))
              (attribute "classname" (class-name testcase))
              (attribute "time" (format nil "~,1f"
                                             (duration testcase)))
              (when-let ((text (error-text testcase)))
                (with-element "error" (cdata text)))
              (when-let ((text (failure-text testcase)))
                (with-element "failure" (cdata text))))))))))

(defun make-junit (&key testsuites)
  (make-instance 'junit-xml :testsuites testsuites))

(defun make-testsuite (name &key testcases timestamp)
  (make-instance 'junit-testsuite
                 :name (princ-to-string name)
                 :testcases testcases :timestamp timestamp))

(defun make-testcase (name class-name duration &key error failure)
  (make-instance 'junit-testcase
                 :name (princ-to-string name)
                 :duration duration
                 :class-name (princ-to-string class-name)
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
