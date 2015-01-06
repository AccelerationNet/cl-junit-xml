# cl-junit-xml

Small library for writing [junit][]-compatible XML files.

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
            problem running the test</error>
        </testcase>
        <testcase name="test" classname="class" time="1.0">
          <failure>
            invalid assertion</failure>
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

### `(write-xml junit sink &key pretty-p)`

writes the junit XML to the given sink. Supports sinks of:

* `nil` - returs the XML as a string
* `T` - writes the XML to `*standard-output*`
* any string - writes the XML to that file, returns the pathname
* any pathname - writes the XML to that file, returns the pathname

if `pretty-p` is non-nil, then the XML produced is indented.

[junit]: http://www.junit.org
