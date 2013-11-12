(module assign-workspace-usage mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  ;; We'll want to reuse this pass.
  (provide assign-workspace-usage)

  (define awu-pt
    (copy-production-table identity0-production-table))
  
  (define-production (err expr)
    (table awu-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all awu
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define PLT (make-hash-table))
  (define >process< (void))
  (define concurrency-state 'any) ;; 'any or 'par
  
  (define-production (mod expr)
    (table awu-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug awu (printf "mod~n"))
      ;; Insert all the processes into the Process Lookup Table
      (for-each
       (lambda (p)
         (if (definition? p)
             (let ([name (name-sym (definition-name p))])
               (debug awu (printf "Inserting process '~a'~n" name))
               (hash-table-put! PLT name p))
             (exn42 catch-all PLT-insert
                    "Really bad."
                    (gm meta 'stx))))
       process+)
      
      ;; Make sure we can use <process> later in the recursion
      (set! >process< <process>)
      
      (let ([pairs (map (lambda (p)
                          (let-values ([(sizes tree)
                                        (<process> p)])
                            (cons sizes tree)))
                        process+)])
        ;; Dump some info out the debug channel.
        (for-each (lambda (p)
                    (if (definition? p)
                        (debug awu
                          (printf "~a sizes: ~a~n" 
                                  (name-sym (definition-name p))
                                  (e&gm p 'workspace-size)))))
                  process+)
                          
        (make-mod meta (map cdr pairs)))
      ]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table awu-pt)
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
    (table awu-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug awu (printf "definition~n"))
      (let-values ([(sizes tree)
                    (<process> process)])
        ;; Allocate one slot for every formal.
        (let ([sizes (cons (length formal*) sizes)])
          (values sizes
                  (im! 
                   (make-definition meta name formal* tree)
                   `((workspace-size ,sizes))))))]
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table awu-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug awu (printf "declaration~n"))
      (let-values ([(sizes tree)
                    (<process> process)])
        (values (cons (length name-type+) sizes)
                (make-declaration meta 
                                  name-type+
                                  tree)))]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table awu-pt)
    (relies-on seq conditional loop par err)
    (matches
     [(? seq? s) (<seq> s)]
     [(? par? s) 
      ;; When we go into PAR branches, we need to 
      ;; know, so we can calculate sizes correctly.
      ;; In particular, W.R.T. instances.
      (let ([old-state concurrency-state])
        (dynamic-wind
         (lambda () (set! concurrency-state 'par))
         (lambda () (<par> s))
         (lambda () (set! concurrency-state old-state))))]
     [(? conditional? s) (<conditional> s)]
     ;; All loops have a parent loop structure
     [(? loop? s) (<loop> s)]
     [err (<err> err)]
     ))
  
  (define-production (specification expr)
    (table awu-pt)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s)  
      ;; When we start processing a definition, set our state to
      ;; 'any'. This happens when an instance recurrs back into the tree.
      (let ([old-state concurrency-state])
        (dynamic-wind
         (lambda () (set! concurrency-state 'any))
         (lambda () (<definition> s))
         (lambda () (set! concurrency-state old-state))))]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table awu-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug awu (printf "instance~n"))
      (let ([the-instance (make-instance 
                           meta
                           name
                           actual*)])
        (case concurrency-state
          ;; This is wrong.
          ;;[(any) (values (list 4) the-instance)]
          [(any par) 
           (debug awu (printf "We're in a PAR state."))
           (let* ([instance-process
                   (hash-table-get PLT (name-sym name))]
                  [instance-size
                   (let-values ([(sizes tree-ignored)
                                 (>process< instance-process)])
                     (apply + sizes))])
             (values (list (+ 4 instance-size)) the-instance))]))]
     [err (<err> err)]
     ))
  
  (define (maximum ls)
    (let ([m -1])
      (for-each (lambda (v) 
                  (if (> v m)
                      (set! m v))) ls)
      m))
  
  (define-production (seq expr)
    (table awu-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug awu (printf "seq~n"))
      ;; Only one value goes back up the tree from a 
      ;; SEQ... this is because we only ever run one of 
      ;; these processes at a time, so we only need
      ;; to reserve as much space as the largest process
      ;; requires.
      (let ([sizes
             (map (lambda (p)
                         (let-values ([(sizes tree)
                                       (<process> p)])
                           sizes)) process+)]
            [trees
             (map (lambda (p)
                         (let-values ([(sizes tree)
                                       (<process> p)])
                           tree)) process+)])
        (let ([max-seq-value (maximum (map (lambda (ls)
                                             (debug awu
                                               (printf "SEQ ls: ~a~n" ls))
                                             (apply + ls)) sizes))])
          (values (list max-seq-value)
                  (make-seq meta trees))))]
     [err (<err> err)]
     ))
  
  (define-production (par expr)
    (table awu-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug awu (printf "par~n"))
      ;; Only one value comes back from a PAR, which
      ;; is the sum of the branches.
      (let ([sizes
             (map (lambda (p)
                    (let-values ([(sizes tree)
                                  (<process> p)])
                      sizes)) process+)]
            [trees
             (map (lambda (p)
                    (let-values ([(sizes tree)
                                  (<process> p)])
                      tree)) process+)])
        (let ([sum-par-processes
               (apply + (map (lambda (ls)
                               (debug awu 
                                 (printf "PAR ls: ~a~n" ls))
                               (apply + ls)) sizes))])
          (values (list sum-par-processes)
                  (im! (make-par meta trees)
                       `((workspace-size
                          ,sizes))
                       ))))]
     [err (<err> err)]
     ))
  
  (define-production (conditional expr)
    (table awu-pt)
    (relies-on choice err)
    (matches
     #;[(struct conditional (meta choice+))
        (debug awu (printf "conditional~n"))
        (make-conditional meta (map <choice> choice+))]
     [err (<err> err)]
     ))
  
  (define-production (choice expr)
    (table awu-pt)
    (relies-on expression process err)
    (matches
     #;[(struct choice (meta e process))
        (debug awu (printf "choice~n"))
        (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table awu-pt)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug awu (printf "loop/while~n"))
      (let-values ([(sizes tree)
                    (<process> process)])
        (values sizes
                ;; Don't process expressions in this pass.
                (make-while meta e tree)))]
     [err (<err> err)]
     ))
  
  (define-production (action expr)
    (table awu-pt)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (values '() s)]
     [(? input? s) (values '(5) s)]
     [(? output? s) (values '(5) s)]
     [err (<err> err)]
     ))
  
  (define-production (assignment expr)
    (table awu-pt)
    (relies-on variable expression err)
    (matches
     #;[(struct assignment (meta var exp))
        (debug awu (printf "assignment~n"))
        (make-assignment meta
                         (<variable> var)
                         (<expression> exp))]
     [err (<err> err)]
     ))
  
  (define-production (input expr)
    (table awu-pt)
    (relies-on chan variable err)
    (matches
    #; [(struct input (meta chan var))
      (debug awu (printf "input~n"))
      (make-input meta (<chan> chan) (<variable> var))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table awu-pt)
    (relies-on chan expression err)
    (matches
     #;[(struct output (meta chan e))
        (debug awu (printf "output~n"))
        (make-output meta (<chan> chan) (<expression> e))]
     [err (<err> err)]
     ))
  
  (define-production (expression expr)
    (table awu-pt)
    (relies-on literal variable err)
    (matches 
     #|
     [(struct monadic-expression (meta rator rand))
      (debug awu (printf "monadic-expression~n"))
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (debug awu (printf "dyadic-expression~n"))
      (make-dyadic-expression meta rator (<expression> rand1) (<expression> rand2))]
     [(? literal? s ) (<literal> s) ]
     [(? variable? s) (<variable> s)]
     |#
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table awu-pt)
    (relies-on err)
    (matches
     #;[(struct literal (meta num))
      (debug awu (printf "literal~n"))
      (make-literal meta num)]
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table awu-pt)
    (relies-on err)
    (matches
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table awu-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug awu (printf "name~n"))
      (make-name meta n)]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table awu-pt)
    (relies-on err)
    (matches
     #;[(struct type (meta t))
      (debug awu (printf "type~n"))
      (make-type meta t)]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table awu-pt)
    (relies-on err)
    (matches
     #;[(struct chan (meta sym))
      (debug awu (printf "chan~n"))
      (make-chan meta sym)]
     [err (<err> err)]
     ))
  
  (define-production (formal expr)
    (table awu-pt)
    (relies-on name type err)
    (matches
     #;[(struct formal (meta name type))
      (debug awu (printf "formal~n"))
      (make-formal meta (<name> name) (<type> type))]
     [err (<err> err)]
     ))
  
  (define assign-workspace-usage
    (make-identity0-grammar
     awu-pt))
  
  )