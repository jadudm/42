(module check-only-top-level-definitions mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))   
  
  ;; Provide the pass
  (provide check-only-top-level-definitions)
  
  (define cotld-pt
    (copy-production-table identity0-production-table))
  
  (define-production (mod expr)
    (table cotld-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (for-each
        (lambda (p)
          (unless (definition? p)
            (exn42 catch-all top-level-defn-check
                   (format
                     (string-append
                       "Only top-level definitions are allowed.~n"
                       "A non-definition found at the top level."))
                   (e&gm p 'stx))))
        process+)]
     [err (<err> err)]
     ))
    
  (define check-only-top-level-definitions
    (make-identity0-grammar cotld-pt)))
