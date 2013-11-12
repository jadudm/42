(module expressions2assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  ;; Provide the pass
  (provide expressions2assembly)
  
  ;; Copy an id0 production table
  (define e2a-pt
    (copy-production-table identity0-production-table))
  
  (define (complex-expression? e)
    (or (monadic-expression? e)
        (dyadic-expression? e)))
  
  
  ;; Lets compile all the expressions into asm forms.
  (define-production (expression expr)
    (table e2a-pt)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug e2a (printf "monadic-expression~n"))
      (compile-monadic-expression meta rator (<expression> rand))]
     
     [(struct dyadic-expression (meta rator rand1 rand2))
      (debug e2a (printf "dyadic-expression~n"))
      ;;(make-dyadic-expression meta rator (<expression> rand1) (<expression> rand2))]
      (compile-dyadic-expression meta
                                 rator 
                                 (<expression> rand1)
                                 (<expression> rand2))]
     
     [(? literal? s ) 
      ;;(<literal> s) 
      (let ([meta (NM s)]
            [num (literal-num s)])
        (debug e2a (printf "literal~n"))
        ;; Literals will always become LDCs.
        (make-assembly*
         meta 
         (make-ldc meta (number->literal meta num))))
      ]
     
     [(? variable? s) 
      ;;(<variable> s)
      (let ([meta (NM s)]
            [var (variable-sym s)])
        (debug e2a (printf "variable~n"))
        (make-assembly* 
         meta
         ;; I don't think we can resolve this yet; non-local
         ;; variables need to have a pointer to a workspace 
         ;; loaded as well, which we don't have at this stage. Therefore,
         ;; <asm> should just allow variables to hang around.
         ;;
         ;; However, we do need to indicate this is a variable reference.
         ;; I'll introduce the 'variable-usage tag.
         (im! s '((variable-usage reference)))
         ))]
     
     [err (<err> err)]
     ))
    
  (define (extract-value struct)
    (cond
      [(variable? struct)
       (variable-sym struct)]
      [(literal? struct)
       (literal-num struct)]))
  
  ;; cme :: monadic-expression -> assembly
  ;; Takes a monadic-expression structure:
  ;; (make-monadic-expression meta rator (<expression> rand))
  ;; and returns an assembly structure. This is in keeping with 
  ;; the id1 grammar. In other words, we are leaving id0.
  ;;
  ;; Monadic expressions are:
  ;; unary minus
  ;; not
  (define (compile-monadic-expression meta rator rand)
    (case rator
      [(-) (make-assembly*
            meta
            rand
            (make-not meta)
            (make-adc meta (number->literal meta 1)))]
      ;; This looks wrong...
      #;[(not) (make-assembly*
                meta
                rand
                (make-eqc meta (number->literal meta 0))
                )]
      [else
       (exn42 expression expressions2assembly
              (internal-compiler-error
               (format
                "No pathway to assembly for the unary '~a' operator.~n"
                rator))
              (gm meta 'stx)
              (gm meta 'pstx))]
      ))
  
  (define (compile-dyadic-expression meta rator rand1 rand2)
    (case rator
      [(+) (make-assembly* meta
                           rand1
                           rand2
                           (make-add meta)
                           )]
      [(-) (make-assembly* meta 
                           ;; Check order on subtraction!
                           rand2
                           rand1
                           (make-diff meta)
                           )]
      [(*) (make-assembly* meta  
                           rand1
                           rand2
                           (make-prod meta)
                           )]
      [(>) (make-assembly* meta
                           rand1 
                           rand2 
                           (make-gt meta))]
      ;; The operands come in the other way round to make it less than.
      [(<) (make-assembly* meta
                           rand2 
                           rand1 
                           (make-gt meta))]
      [else
       (exn42 expression expressions2assembly
              (internal-compiler-error
               (format
                "No pathway to assembly for the dyadic '~a' operator.~n"
                rator))
              (gm meta 'stx)
              (gm meta 'pstx))]
      ))
  
  ;; I suspect every pass should override <err>...
  (define-production (err expr)
    (table e2a-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all expressions2assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
               
  
  ;; Define the pass
  (define expressions2assembly
    (make-identity0-grammar e2a-pt))
  
  )
