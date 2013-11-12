(module seq-par2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity1.scm" "42"))
  
  ;; I've suddenly remembered that PAR
  ;; took actual thought.
  
  ;; I think I'll take a break for today, after all.
  
  
  ;; Provide the pass
  (provide seq-par2assembly)
  
  ;; Copy an id1 production table
  (define seq-par-pt
    (copy-production-table identity1-production-table))
  
  (define-production (seq exp)
    (table seq-par-pt)
    (relies-on process err)
    (matches
     [any
      (debug sp2a 
        (printf "SEQ~n"))
      any]))
  
  (define-production (par exp)
    (table seq-par-pt)
    (relies-on process err)
    (matches
     [any 
      (debug sp2a 
        (printf "PAR~n"))
      any]))
  
  
  (define seq-par2assembly (make-identity1-grammar seq-par-pt))
  )