(module check-only-top-level-procs mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))   
  
  ;; Provide the pass
  (provide check-only-top-level-procs)
  
  (define cotlp-pt
    (copy-production-table identity0-production-table))
  
  (define at-top-level? #t)
  
  (define-production (mod expr)
    (table cotlp-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table identity0-production-table*)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (set! at-top-level? #f) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [err (<err> err)]
     ))
  
  (define check-only-top-level-procs (make-identity0-grammar cotlp-pt)))
