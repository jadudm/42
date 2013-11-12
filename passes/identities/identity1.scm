(module identity1 mzscheme
  (require (lib "util/standard-includes.scm" "42")
           ;; Import everything except processes we're overriding.
           (all-except (lib "passes/identities/identity0.scm" "42")
                       process@
                       mod@
                       identity0))

  (provide 
   ;; Provide the pass and the table
   identity1
   identity1-production-table
   make-identity1-grammar
   )
  
  (define identity1-production-table*
    (copy-production-table identity0-production-table))
  
  ;; Define the productions we are overriding
  (define-production (err expr)
    (table identity1-production-table*)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all identity1
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define-production (choice expr)
    (table identity1-production-table*)
    (relies-on asm process err)
    (matches
     [(struct choice (meta e process))
      (debug identity1 (printf "choice~n"))
      (make-choice meta (<asm> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table identity1-production-table*)
    (relies-on asm process err)
    (matches
     [(struct while (meta e process))
      (debug identity1 (printf "loop/while~n"))
      (make-while meta (<asm> e) (<process> process))]
     [err (<err> err)]
     ))

  (define-production (assignment expr)
    (table identity1-production-table*)
    (relies-on variable asm err)
    (matches
     [(struct assignment (meta var exp))
      (debug identity1 (printf "assignment~n"))
      (make-assignment meta
                       (<variable> var)
                       (<asm> exp))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table identity1-production-table*)
    (relies-on chan asm err)
    (matches
     [(struct output (meta chan e))
      (debug identity1 (printf "output~n"))
      (make-output meta (<chan> chan) (<asm> e))]
     [err (<err> err)]
     ))
  
  (define-production (asm expr)
    (table identity1-production-table*)
    (relies-on literal variable err)
    (matches
     [(struct assembly (meta instructions+))
      (make-assembly meta (map <asm> instructions+))]
     [(? primary? expr)
      (debug identity1 (printf "primary~n")) expr]
     [(? secondary? expr) (debug identity1 
                            (printf "secondary~n"))
                          expr]
     [(? variable? v) v]
     [err (<err> err)]
     ))
  
  
  
  ;; Replaces all <expression> forms with <asm> forms.
  (define make-identity1-grammar
    (make-pass
     ([mod process err]
      [process action construction instance specification err]
      [definition name formal process err]
      [declaration name type process err]      
      [construction seq conditional loop  par err]
      [specification declaration definition err]
      [instance name err]
      [seq process err]
      [par process err]
      [conditional choice err]
      [choice asm process err]
      [loop asm process err]
      [action assignment input output err]
      [assignment variable asm err]
      [input chan variable err]
      [output chan asm err]
      [asm literal variable err]
      [literal err]
      [variable err]
      [name err]
      [type err]
      [chan err]
      [formal name type err]
      [err]
        )))
  
  (define identity1-production-table
    (make-table-immutable identity1-production-table*))
  
  (define identity1 
    (make-identity1-grammar identity1-production-table*))

  )