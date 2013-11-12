(module loop2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity2.scm" "42"))  
  
  ;; Provide the pass
  (provide loop2assembly)
  
  (define loop-pt
    (copy-production-table identity2-production-table))
  
  (define-production (loop expr)
    (table loop-pt)
    (relies-on asm process err)
    (matches
     [(struct while (meta e process))
      (debug loop (printf "loop/while~n"))
      (let ([loop-start (generate-unique-label meta 'loop-start)]
            [loop-end (generate-unique-label meta 'loop-end)])
        
      (make-assembly* meta 
                      loop-start
                      (<asm> e)
                      (make-cj meta loop-end)
                      (<process> process)
                      (make-j meta loop-start)
                      loop-end))]
      ;;(make-while meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (err expr)
    (table loop-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all loop2assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  
  (define loop2assembly (make-identity2-grammar loop-pt))
  )