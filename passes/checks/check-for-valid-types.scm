(module check-for-valid-types mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide check-for-valid-types)
  
  (define cfvt-production-table
    (copy-production-table identity0-production-table))
  
  ;; Here we want to check if t is 
  ;; a valid type symbol.
  (define-production (type expr)
    (table cfvt-production-table)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug cfvt (printf "type~n"))
      (if (member t '(int chan))
          (make-type meta t)
          (exn42 type invalid 
                 (format 
                  (tab-append
                   "'~a' is not a valid type.")
                  t)
                 (gm meta 'stx)
                 (gm meta 'pstx)))]
     [err (<err> err)]
     ))
  
  ;; This uses the identity0 grammar-maker to instantiate the
  ;; grammar of identity0 here-and-now. This way, we can 
  ;; change that grammar later, and things should still work.
  ;; That is, we can add a new production to identity0, and 
  ;; it will be instantiated here, properly, without any effort on 
  ;; our part. While I had considered doing this in the past,
  ;; thanks go to Axel Simon for encouraging me to give the macros
  ;; a look.
  
  ;;  (insert-productions identity0-production-table type)
  
  (define check-for-valid-types (make-identity0-grammar cfvt-production-table))
  
  )