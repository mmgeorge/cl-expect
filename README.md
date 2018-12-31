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
TBC

## License 
MIT
