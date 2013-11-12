(module identity0 mzscheme
  (require (lib "util/standard-includes.scm" "42"))
  
  ;; We'll want to reuse this pass.
  (provide identity0 
           identity0-production-table
           make-identity0-grammar)
  
  
  
  (define identity0-production-table*
    (copy-production-table empty-production-table))
  
  (define-production (err expr)
    (table identity0-production-table*)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all id0
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define-production (mod expr)
    (table identity0-production-table*)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug id0 (printf "mod~n"))
      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table identity0-production-table*)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [err (<err> err)]
     ))
  
  (define-production (definition expr)
    (table identity0-production-table*)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug id0 (printf "definition~n"))
      (make-definition meta 
                       (<name> name) 
                       (map <formal> formal*)
                       (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table identity0-production-table*)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug id0 (printf "declaration~n"))
      (make-declaration meta 
                        (map 
                         (lambda (pair)
                           (list 
                            (<name> (car pair))
                            (<type> (cadr pair))))
                         name-type+)
                        (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table identity0-production-table*)
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
    (table identity0-production-table*)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s) (<definition> s)]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table identity0-production-table*)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug id0 (printf "instance~n"))
      (make-instance meta (<name> name) (map <name> actual*))]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table identity0-production-table*)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug id0 (printf "seq~n"))
      (make-seq meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (par expr)
    (table identity0-production-table*)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug id0 (printf "par~n"))
      (make-par meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (conditional expr)
    (table identity0-production-table*)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug id0 (printf "conditional~n"))
      (make-conditional meta (map <choice> choice+))]
     [err (<err> err)]
     ))
  
  (define-production (choice expr)
    (table identity0-production-table*)
    (relies-on expression process err)
    (matches
     [(struct choice (meta e process))
      (debug id0 (printf "choice~n"))
      (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table identity0-production-table*)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug id0 (printf "loop/while~n"))
      (make-while meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (action expr)
    (table identity0-production-table*)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (<assignment> s)]
     [(? input? s) (<input> s)]
     [(? output? s) (<output> s)]
     [err (<err> err)]
     ))
  
  (define-production (assignment expr)
    (table identity0-production-table*)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp))
      (debug id0 (printf "assignment~n"))
      (make-assignment meta
                       (<variable> var)
                       (<expression> exp))]
     [err (<err> err)]
     ))
  
  (define-production (input expr)
    (table identity0-production-table*)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug id0 (printf "input~n"))
      (make-input meta (<chan> chan) (<variable> var))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table identity0-production-table*)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan e))
      (debug id0 (printf "output~n"))
      (make-output meta (<chan> chan) (<expression> e))]
     [err (<err> err)]
     ))
  
  (define-production (expression expr)
    (table identity0-production-table*)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug id0 (printf "monadic-expression~n"))
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (debug id0 (printf "dyadic-expression~n"))
      (make-dyadic-expression meta rator (<expression> rand1) (<expression> rand2))]
     [(? literal? s ) (<literal> s) ]
     [(? variable? s) (<variable> s)]
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table identity0-production-table*)
    (relies-on err)
    (matches
     [(struct literal (meta num))
      (debug id0 (printf "literal~n"))
      (make-literal meta num)]
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table identity0-production-table*)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug id0 (printf "variable~n"))
      (make-variable meta var)]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table identity0-production-table*)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug id0 (printf "name~n"))
      (make-name meta n)]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table identity0-production-table*)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug id0 (printf "type~n"))
      (make-type meta t)]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table identity0-production-table*)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug id0 (printf "chan~n"))
      (make-chan meta sym)]
     [err (<err> err)]
     ))
  
  (define-production (formal expr)
    (table identity0-production-table*)
    (relies-on name type err)
    (matches
     [(struct formal (meta name type))
      (debug id0 (printf "formal~n"))
      (make-formal meta (<name> name) (<type> type))]
     [err (<err> err)]
     ))
  
  (define make-identity0-grammar
    (make-pass
     ([mod process err]
      [process action construction instance specification err]
      [definition name formal process err]
      [declaration name type process err]      
      [construction seq conditional loop par err]
      [specification declaration definition err]
      [instance name err]
      [seq process err]
      [par process err]
      [conditional choice err]
      [choice expression process err]
      [loop expression process err]
      [action assignment input output err]
      [assignment variable expression err]
      [input chan variable err]
      [output chan expression err]
      [expression literal variable err]
      [literal err]
      [variable err]
      [name err]
      [type err]
      [chan err]
      [formal name type err]
      [err]
      )))
  
  ;; Provide a protected/immutable table
  (define identity0-production-table
    (make-table-immutable identity0-production-table*))
  
  ;; Create the pass using the id0 grammar and the production table
  ;; from this module.
  (define identity0 
    (make-identity0-grammar
     identity0-production-table))
  
  )