(module conditional2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity2.scm" "42"))
  
  ;; Provide the pass
  (provide conditional2assembly)
  
  ;; Copy an id1 production table
  (define c2a-pt
    (copy-production-table identity2-production-table))
  
  (define-production (conditional expr)
    (table c2a-pt)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug c2a (printf "conditional~n"))
      ;;generate-unique-label
      (let ([end-label (generate-unique-label meta 'end-if)])
        ;; should work magically. - make-assembly* 
        ;; flattens the choice assembly list that is returned
        ;; by map.
        (make-assembly* meta 
                        (map (lambda (c)
                               (<choice> c end-label))
                             choice+)
                        end-label))]
     [err (<err> err)]
     ))
  
  (define-production (choice exp end-label)
    (table c2a-pt)
    (relies-on asm process err)
    (matches
     [(struct choice (meta e process))
      (debug c2a (printf "choice~n"))
      (let ([else-label (generate-unique-label meta 'if)])
        (make-assembly* meta 
                        (<asm> e)
                        (make-cj meta else-label)
                        (<process> process)
                        (make-j meta end-label)
                        else-label))]
     [err (<err> err)]
     ))
  
  (define-production (err expr)
    (table c2a-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all conditional2assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define conditional2assembly 
    (make-identity2-grammar c2a-pt))
  )
