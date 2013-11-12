(module assignment2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity1.scm" "42"))
  
  ;; Provide the pass
  (provide assignment2assembly)
  
  ;; Copy an id1 production table
  (define a2a-pt
    (copy-production-table identity1-production-table))
    
  
  ;; x := y  => ldl y; stl x;
  ;; or 
  ;; x := e  => e; stl x;
  ;;
  ;; If 'x' is non-local (we've tagged it in a previous pass
  ;; with the metatag 'variable-locality), we should do a STNL.
  ;; 
  ;; Also, there are no expressions left at this point,
  ;; assuming that 'expression2assembly' was run first.
  ;; So, x := e would really be
  ;;
  ;; x := <asm>
  ;; 
  ;; which is just fine. We just lift out that assembly,
  ;; and do it before the store.
  (define-production (assignment exp)
    (table a2a-pt)
    (relies-on variable asm err)
    (matches
     ;; At this point, the 'var' slot has a LDL/LDNL in
     ;; it, and the 'e' slot of the struct:assignment 
     ;; contains a struct:assembly, which is just a list
     ;; of struct:assembly or 'struct:asm's.
     [(struct assignment (meta variable asm-list))
      (make-assembly* 
       meta ;; the old assignment metadata!
       asm-list
       ;; Store last!
       ;; And note, we are VERY concerned about our STNL. That's probably wrong.
       ;; XXX, WARNING, and the like. We have picked some metadata and used it,
       ;; but it is almost certainly NOT correct.
       (if (equal? (e&gm variable 'variable-locality) 'local)
           (make-stl (NM variable) 
                     (number->literal meta (e&gm variable 'workspace-location)))
           (make-stnl (NM variable) 
                      (number->literal meta (e&gm variable 'offset-workspace-location))))
       ;; The 'if' replaced this expression
       ;;(im! variable '((variable-usage assignment)))
       )]
     [err (<err> err)]))
      
  (define-production (err expr)
    (table a2a-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all assignment2assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define assignment2assembly (make-identity1-grammar a2a-pt))
  )