(module identity2 mzscheme
  (require (lib "util/standard-includes.scm" "42")
           ;; Import everything except processes we're overriding.
           (only (lib "passes/identities/identity1.scm" "42")
                 identity1-production-table
                 ))

  (provide 
   ;; Provide the pass and the table
   identity2
   identity2-production-table
   make-identity2-grammar
   )
  
  (define identity2-production-table*
    (copy-production-table identity1-production-table))
  
  ;; Define the productions we are overriding
  (define-production (err expr)
    (table identity2-production-table*)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all identity2
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  
  (define-production (process expr)
    (table identity2-production-table*)
    (relies-on action construction instance specification asm err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [(? assembly? s) (<asm> s)]
     [err (<err> err)]
     ))
  
  (define-production (asm expr)
    (table identity2-production-table*)
    (relies-on process literal variable err)
    (matches
     [(? process? s) 
      (debug id2 (printf "Process in ASM.~n"))
      (<process> s)]
     [(struct assembly (meta instructions+))
      (make-assembly meta (map <asm> instructions+))]
     [(? primary? expr)
      (debug id2 (printf "primary~n")) expr]
     [(? secondary? expr) (debug id2 
                            (printf "secondary~n"))
                          expr]
     [(? variable? v) v]
     [err (<err> err)]
     ))
  
  
  ;; Replaces all <expression> forms with <asm> forms.
  (define make-identity2-grammar
    (make-pass
     ([mod process err]
      [process action construction instance specification asm err]
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
      [asm process literal variable err]
      [literal err]
      [variable err]
      [name err]
      [type err]
      [chan err]
      [formal name type err]
      [err]
        )))
  
  (define identity2-production-table
    (make-table-immutable identity2-production-table*))
  
  (define identity2
    (make-identity2-grammar identity2-production-table*))

  )