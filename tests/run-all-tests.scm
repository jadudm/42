;; This module, when executed, runs all the unit tests for the compiler.
;; From the command line:
;;
;; mzscheme -t-- run-all-tests.scm
;;
;; You can also run them in DrScheme under the "module" language.

(module run-all-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 0)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 0)))
  
  (require "all-42-tests.scm")
  
  (print-struct #t)
  
  (test/text-ui all-42-tests)
  (exit)
)