(module procs2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity2.scm" "42"))   
  
  ;; Provide the pass
  (provide procs2assembly)
  
  (define proc-pt
    (copy-production-table identity2-production-table))
  
  
  
  (define-production (err expr)
    (table proc-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all procs2assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  
  (define procs2assembly (make-identity2-grammar proc-pt))
  )