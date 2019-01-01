# Overview
cl-expect is a libary for testing Common Lisp projects using [`asdf:package-inferred-system`](https://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html). If you are not familiar with package-inferred-system, it allows for automatic inferred builds assuming a one-package-per-file project structure. For a good guide on getting started with `package-inferred-system` check out [this](https://davazp.net/2014/11/26/modern-library-with-asdf-and-package-inferred-system.html) guide.

Current development is very much pre-alpha, and several breaking changes should be expected!

## Basic Usage
```
EXAMPLE/FOO> (expect:run-tests)
[FAIL] example/foo:add [1/2]     
[FAIL] example/foo:silly-incr [2/3]

Test of example/foo:add [Add two numbers - bad test, should fail!] failed [1] expects:  
   - (eq (add 0 1) 2) -> (eq (add 0 1) 2)  
      - 1 does not eq 2 

Test of example/foo:silly-incr [Call with bad args - bad test, should fail!] failed [1] expects:
   - (eq (silly-incr 4 5) 9) -> (eq (silly-incr 4 5) 9)
      - Unexpected simple-error: Got a value that is not 1!
        - (error "Got a value that is not 1!") [COMMON-LISP]
        - (check-value-is-1 5)                 [EXAMPLE/FOO(foo.lisp:10)]
        - (silly-incr 4 5)                     [EXAMPLE/FOO(foo.lisp:16)] 
        
Ran with failures: [3/5]
```
## Reference 
**deftest-of** _function-name lambda-list docstring form*_

Create a new test. One or more `expect` forms should be included within the body of the test. 
- _function-name_ the name of the tested function
- _lambda-list_ an empty list (reserved for future use)
- _docstring_ a **required** description of the test
- _form_* one or more forms, at least one of which must be an `expect`
```common-lisp
(deftest-of add ()
  "Addition of two values"
  (expect (eq (add 1 2) 3)))
```
---
**expect** _form_

Create a new assertion for _form_. When _test_ evaluates to T the test passed, otherwise fails. Rather than provide a libarary of designated predicates, `expect` is designed to be used with any user-provided form. It then unwraps and safely evaluates the expression. If an error is thrown, a restart is triggered and the stack is unwrapped. A stack trace, along with the test-failure, will then appear in the test report. 


An expect must appear within a `deftest-of` body otherwise an error will be thrown. 
- _form_ a form to evaluate which must evaluate to non-NIL on test success. 
```common-lisp
(deftest-of divide ()
  "Some descriptive test name"
  (expect (eq (divide 2 1) 2))
  (expect (typep (divide 2 0) 'division-by-zero)))
```
---
**run-tests** &optional (_package-or-package-name-or-system-name_ \*package\*)

Find and run all tests for _package-or-package-name-or-system-name_.

When a `package` or `package-name` is given, then the corresponding `PACKAGE-NAME.test.lisp` file will be run provided the file exists. When no argument is provided, the current package is run. If the file does not exist, any definitions for tests currently loaded in the system will be run instead. This means that `deftest-of` forms may appear intermixed with source code, or use `make-test-file` to create a dedicated file for a package's tests. When a `system-name` is provided, `run-tests` will search for correpsonding test definitions for each package. 

```common-lisp
(run-tests) ;; Run tests for the current package
(run-tests :suite/foo) ;; Run tests for suite/foo
(run-tests "suite/foo") ;; Run tests for suite/foo
(run-tests "suite") ;; Run tests for every package of suite
```
---
**clear-tests** &optional (_package-or-package-name-or-system-name_ \*package\*)

Clear all currently loaded tests for _package-or-package-name-or-system-name_. Useful for un-loading a test definition that has been deleted. Usage is similar to `run-tests`

---
**make-test-file** &optional (_package_ \*package\*)

Create a corresponding test file for the given package. 

When a `package` is given, a corresponding `PACKAGE-NAME.test.lisp` file will be generated at the path at which the package's file is located. When no argument is provided, the test file is generated for the current package. When calling `run-tests`, the test runner will search for correponding test files and load them. Making changes to these files will cause them to be reloaded. 

```common-lisp
(make-test-file) ;; Make a test file for the current package
```

## License 
MIT
