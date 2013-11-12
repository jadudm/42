(module tag-workspace-usage mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))  
  
  ;; Provide the pass
  (provide tag-workspace-usage)
  
  (define twu-pt
    (copy-production-table identity0-production-table))
  
  (define-production (err expr)
    (table twu-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all assign-workspace-usage
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define-production (mod expr)
    (table twu-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug twu (printf "mod~n"))
      (let ([usage-struct-pairs
             (map (lambda (p)
                    (let-values ([(usage struct)
                                  (<process> p)])
                      (cons usage struct))) process+)])
        (debug twu
          (printf "Module usage: ~a~n" (apply append (map car usage-struct-pairs))))
        (make-mod meta (map cdr usage-struct-pairs)))]
     [err (<err> err)]
     ))
  
  
  (define-production (process expr)
    (table twu-pt)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) (values '() s)]
     [(? skip? s) (values '() s)]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [err (<err> err)]
     ))
  
  
  (define-production (definition expr)
    (table twu-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug twu (printf "definition~n"))
      (let-values ([(usage process-struct)
                    (<process> process)])
        (let ([formal-usage-pairs
                    (map (lambda (f)
                           (let-values ([(use st)
                                        (<formal> f)])
                             (cons use st)))
                         formal*)])
        
        ;; Extend the usage with the formals?
        (set! usage (append (map car formal-usage-pairs)
                            usage))
        
        (debug twu
          (printf "definition '~a' usage: ~a~n" 
                  (name-sym name) usage))
        (values
         usage
         (im! 
          (make-definition meta 
                           (<name> name) 
                           (map cdr  formal-usage-pairs)
                           process-struct)
          `((workspace-usage ,usage))))))]
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table twu-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug twu (printf "declaration~n"))
      (let-values ([(usage pstruct)
                    (<process> process)])
        (values
         ;; WARNING May become a problem with more complex types later.
         (append (map (lambda (pair) 
                        (cons (type-sym (cadr pair))
                              (name-sym (car pair))
                              )) name-type+) usage)
         (make-declaration meta 
                           (map 
                            (lambda (pair)
                              (list 
                               (<name> (car pair))
                               (<type> (cadr pair))))
                            name-type+)
                           pstruct)))]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table twu-pt)
    (relies-on seq conditional loop par err)
    (matches
     [(? seq? s) (<seq> s)]
     [(? conditional? s) (<conditional> s)]
     ;; All loops have a parent loop structure
     [(? loop? s) (<loop> s)]
     [(? par? s) (<par> s)]
     [err (<err> err)]
     ))
  
  (define-production (specification expr)
    (table twu-pt)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s) (<definition> s)]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table twu-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug twu (printf "instance~n"))
      (values (list (cons 'call (name-sym name)))
              (make-instance meta (<name> name) (map <name> actual*)))]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table twu-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug twu (printf "seq~n"))
      (let ([usage-struct-pairs
             ;; First, gather up the usage symbols.
             (map (lambda (p)
                    (let-values ([(usage struct)
                                  (<process> p)])
                           (cons usage struct)))
                  process+)])
        (let ([usage-metadata
               (map car usage-struct-pairs)])
          
          (values
           (apply append usage-metadata)
           (im! (make-seq meta (map cdr usage-struct-pairs))
                `((workspace-usage ,usage-metadata)))
           )))]
                         
     [err (<err> err)]
     ))
  
  (define-production (par expr)
    (table twu-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug twu (printf "par~n"))
      ;; This might be weird. So, a par is a process unto itself. Therefore,
      ;; if it contains a par, then ... LEFT OFF HERE...
      (let ([usage-struct-pairs
             ;; First, gather up the usage symbols.
             (map (lambda (p)
                    (let-values ([(usage tree)
                                  (<process> p)])
                           (cons usage tree)))
                  process+)])
        
        (debug twu
          (printf "Par usage (per branch):~n")
          (for-each
           (lambda (ls)
             (printf " : ~a~n" ls))
           (map car usage-struct-pairs)))
        
        (let ([usage-metadata
               ;; Used to apply/append this data. I don't want to
               ;; when I'm storing it, but I want to when
               ;; I pass it back up the tree.
               (map car usage-struct-pairs)])
          (values
           ;; Here, we flatten the PAR data.
           ;; But we need to indicate *which* par is in the workspace. So, use 
           ;; the globally unique nodeID instead of the symbol 'par'. This will
           ;; be problematic in some cases, but we can manage it.
           (append (apply append usage-metadata) (list (cons 'par (gm meta 'nodeID))))
           (im! (make-par meta (map cdr usage-struct-pairs))
                `((workspace-usage ,usage-metadata)))
           )))]
     [err (<err> err)]
     ))
  
  #| MCJ 20060706 thinks this is broken, but not being exercised, at the moment
  (define-production (conditional expr)
    (table twu-pt)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug twu (printf "conditional~n"))
      (make-conditional meta (map <choice> choice+))]
     [err (<err> err)]
     ))
  
  
  (define-production (choice expr)
    (table twu-pt)
    (relies-on expression process err)
    (matches
     [(struct choice (meta e process))
      (debug twu (printf "choice~n"))
      (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  |#
  
  (define-production (loop expr)
    (table twu-pt)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug twu (printf "loop/while~n"))
      (let-values ([(usage pstruct)
                    (<process> process)])
        (values usage
                (make-while meta (<expression> e) pstruct)))]
     [err (<err> err)]
     ))

  
  (define-production (action expr)
    (table twu-pt)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (values '() (<assignment> s))]
     [(? input? s) (<input> s)]
     [(? output? s) (<output> s)]
     [err (<err> err)]
     ))
  
  #|
  (define-production (assignment expr)
    (table twu-pt)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp))
      (debug twu (printf "assignment~n"))
      (values '()
              (make-assignment meta
                               (<variable> var)
                               (<expression> exp)))]
     [err (<err> err)]
     ))
  |#
  
  (define-production (input expr)
    (table twu-pt)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug twu (printf "input~n"))
      (values '(input)
              (make-input meta (<chan> chan) (<variable> var)))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table twu-pt)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan e))
      (debug twu (printf "output~n"))
      (values '(output)
              (make-output meta (<chan> chan) (<expression> e)))]
     [err (<err> err)]
     ))
  
  #|
  (define-production (expression expr)
    (table twu-pt)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug twu (printf "monadic-expression~n"))
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (debug twu (printf "dyadic-expression~n"))
      (make-dyadic-expression meta rator (<expression> rand1) (<expression> rand2))]
     [(? literal? s ) (<literal> s) ]
     [(? variable? s) (<variable> s)]
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table twu-pt)
    (relies-on err)
    (matches
     [(struct literal (meta num))
      (debug twu (printf "literal~n"))
      (make-literal meta num)]
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table twu-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug twu (printf "variable~n"))
      (make-variable meta var)]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table twu-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug twu (printf "name~n"))
      (make-name meta n)]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table twu-pt)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug twu (printf "type~n"))
      (make-type meta t)]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table twu-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug twu (printf "chan~n"))
      (make-chan meta sym)]
     [err (<err> err)]
     ))
|#
  
  (define-production (formal expr)
    (table twu-pt)
    (relies-on name type err)
    (matches
     [(struct formal (meta name type))
      (debug twu (printf "formal~n"))
      (values (cons 'formal (name-sym name))
              (make-formal meta (<name> name) (<type> type)))]
     [err (<err> err)]
     ))
  
  (define tag-workspace-usage (make-identity0-grammar twu-pt))
  )