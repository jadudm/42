(module all-42-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 0)))
  ;;(require (lib "test.ss" "schemeunit"))
  (require "syntax-error-tests.scm")
  
  (provide all-42-tests)

  (define all-42-tests
    (test-suite 
     "Suite 42" 
     syntax-error:type
     syntax-error:definition
     syntax-error:variable
     syntax-error:instance
     ))
 )
