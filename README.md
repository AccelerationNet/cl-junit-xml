# cl-junit-xml

Small library for writing [junit][]-compatible XML files.

[junit]: http://www.junit.org

## Example

    CL-JUNIT-XML> (let* ((junit (make-junit))
           (suite (add-child junit (make-testsuite "suite" :timestamp "now"))))
      (add-child suite (make-testcase "test" "class" 1.0
                                      :failure "invalid assertion"))
      (add-child suite (make-testcase "test 2" "class" 2.0
                                      :error "problem running the test"))
      (add-child suite (make-testcase "test 3" "class" 5.0))
      (write-xml junit T :pretty-p T))
    <?xml version="1.0" encoding="UTF-8"?>
    <testsuites>
      <testsuite name="suite" package="" timestamp="now" id="0" tests="3" errors="1" failures="1" time="8.0">
        <testcase name="test 3" classname="class" time="5.0"/>
        <testcase name="test 2" classname="class" time="2.0">
          <error>
            <![CDATA[problem running the test]]></error>
        </testcase>
        <testcase name="test" classname="class" time="1.0">
          <failure>
            <![CDATA[invalid assertion]]></failure>
        </testcase>
      </testsuite>
    </testsuites>


## API

### `make-junit`

creates a new junit root XML object you can add suites to

### `make-testsuite`

creates a new junit testsuite add testcases to

### `make-testcase`

creates a new junit testcase

### `(add-child parent child)`

add cases to suites, and suites to junit. returns the child

### `(write-xml junit sink &key pretty-p &allow-other-keys)`

writes the junit XML to the given sink. Supports sinks of:

* `nil` - returs the XML as a string
* `T` - writes the XML to `*standard-output*`
* any string - writes the XML to that file, returns the pathname
* any pathname - writes the XML to that file, returns the pathname

if `pretty-p` is non-nil, then the XML produced is indented.

Other keys are allowed to support integration with other testing
libraries.

## Integration with other testing libraries

### [lisp-unit2][]

[lisp-unit2][] support is available via the `cl-junit-xml.lisp-unit2`
ASDF system, and adds some additional `write-xml` specializations for
[lisp-unit2][] objects.

#### writing XML directly

    (write-xml (lisp-unit2:run-tests :name :my-tests) T :pretty-p T)

#### writing XML via the [lisp-unit2][]'s signals:

    (handler-bind
        ((lisp-unit2:all-tests-complete
           #'(lambda (c)
               (write-xml c T :pretty-p T))))
      (lisp-unit2:run-tests :name :my-tests))

[lisp-unit2]: https://github.com/AccelerationNet/lisp-unit2

### [lisp-unit][]

[lisp-unit][] support is available via the `cl-junit-xml.lisp-unit`
ASDF system, and adds some additional `write-xml` specializations for
[lisp-unit][] objects.

#### writing XML via the [lisp-unit][]'s signals:

    (handler-bind
        ((lisp-unit:tests-run-complete
           #'(lambda (c)
           (write-xml c T :pretty-p T))))
      (lisp-unit:signal-results)
      (lisp-unit:run-tests))

[lisp-unit]: http://www.cliki.net/lisp-unit
