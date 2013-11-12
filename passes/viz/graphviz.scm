(module graphviz mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  ;; We'll want to reuse this pass.
  (provide graphviz)
  
  (define gv-pt
    (copy-production-table identity0-production-table))
 

  (define-production (err expr)
    (table gv-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all id0
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define (make-gviz-filestring  str)
    (string-append
     "digraph G {~n"
     "graph [ overlap=compress " "splines=true " "rankdir=LR ]~n"
     str
     "~n}~n"))
  
  (define workspace-usage-table (void))
    
  
  (define-production (mod expr)
    (table gv-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (set! workspace-usage-table (gm meta 'workspace-usage-table))
      
      (debug graphviz
        (let ([dir (get-gmeta 'graphviz-directory)])
          ;; Check if the directory exists; make it if it doesn't.
          (if (not (directory-exists? dir))
              (make-directory dir))

          (for-each
           (lambda (p pname)
             (let ([op
                    (open-output-file
                     (format "~a/~a-graphviz.dot" dir pname) 'replace)])
               (parameterize ([current-output-port op])
                             (printf (make-gviz-filestring (<process> p))))
               (close-output-port op))
             (printf (make-gviz-filestring (<process> p))))
           process+
           (map (lambda (p)
                  (if (definition? p)
                      (stxexpand (definition-name p))
                      (gensym 'anon-proc)))
                process+)
          ))) ;; End of debug
      
      expr]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table gv-pt)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) (label (e&gm s 'nodeID) 'stop)]
     [(? skip? s) (label (e&gm s 'nodeID) 'skip)]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [err (<err> err)]
     ))
  
  (define connect 
    (case-lambda
     [(sym o)
      (cond
       [(node? o)
        (format "~a -> ~a;~n" sym (e&gm o 'nodeID))]
       [(symbol? o)
        (format "~a -> ~a;~n" sym o)]
       [(string? o)
        (format "~a -> ~a;~n" sym o)])]
     [(sym o label)
      (cond
       [(node? o)
        (format "~a -> ~a [label=\"~a\"];~n" sym (e&gm o 'nodeID) label)]
       [(symbol? o)
        (format "~a -> ~a [label=\"~a\"];~n" sym o label)]
       [(string? o)
        (format "~a -> ~a [label=\"~a\"];~n" sym o label)])]
     ))

  (define (label sym lab)
    (format "~a [label = \"~a\"]~n" sym lab))
  
  #;(define (list-intersperse ls o)
    (cond
      [(null? ls) '()]
      [(null? (cdr ls) (cons o ls))]
      [else
       (cons (car ls)
             (cons o
                   (list-intersperse (cdr ls) o)))]))
  
  (define (record id lop)
    (string-append 
     (format "~a [shape=record, label=\"" id)
     (apply string-append
            (list-intersperse 
             (map (lambda (p)
                    (format "{~a | ~a}" (car p) (cadr p)))
                 lop) "|"))
     (format "\"];~n")))
  
  (define-production (definition expr)
    (table gv-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug gv (printf "definition~n"))
      (let ([thisID (gm meta 'nodeID)]
            [metaID (gensym 'meta)])
        (string-append
         (apply string-append
                (map (lambda (f) 
                       (let ([type (formal-type f)])
                         ;; Put the types on the connections
                         (connect thisID f))) formal*))
         (connect thisID process)
         (label thisID (stxexpand name))
         (apply string-append (map <formal> formal*))
         (<process> process)
         ;; Metadata
         (record metaID `((PS ,(gm meta 'process-size))
                          (AJW ,(gm meta 'ajw-offset))
                          ;; WSU is a big list...
                          ;;(WSU ,(gm meta 'workspace-usage))
                          ))
         (connect thisID metaID)
         ))]
     
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table gv-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug gv (printf "declaration~n"))
      (let ([thisID (gm meta 'nodeID)])
          (string-append
           (apply string-append 
                  (map (lambda (n t)
                         (let ([pairID (gensym 'pair)])
                           (string-append
                            (record pairID `((,(stxexpand n) 
                                               ,(type-sym t))
                                             (loc ,(e&gm n 'workspace-location))
                                             ))
                            (connect thisID pairID))))
                       (map car name-type+)
                       (map cadr name-type+)))
           (connect thisID process)
           (<process> process)
           ))]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table gv-pt)
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
    (table gv-pt)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s) (<definition> s)]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table gv-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug gv (printf "instance~n"))
      (let ([thisID (gm meta 'nodeID)])
        (string-append
         (apply string-append
                (map (lambda (n)
                       (let ([actualID (gensym 'actual)])
                         (string-append
                          (connect thisID n)
                          (<name> n)
                          )))
                        actual*))
         ;; Just make the instance a record
         (record thisID `((inst ,(stxexpand name))
                          (IU ,(hash-table-get workspace-usage-table
                                              (name-sym name)))))
         ))]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table gv-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug gv (printf "seq~n"))
      (let ([thisID (gm meta 'nodeID)])
        (apply string-append
               (append
                (map (lambda (p)
                       (connect thisID p))
                     process+)
                (map <process> process+))))]
     [err (<err> err)]
     ))
  
  (define-production (par expr)
    (table gv-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug gv (printf "par~n"))
        (let ([thisID (gm meta 'nodeID)]
              [metaID (gensym 'meta)])
          (apply string-append
                 (append
                  (map (lambda (p pbwu)
                         (connect thisID p (format "~a" pbwu)))
                       process+ (gm meta 'par-branch-workspace-usage))
                  (map <process> process+)
                  (list
;;                   (record metaID 
;;                           `((PBWU ,(gm meta 'par-branch-workspace-usage))
;;                             (P ,(map (lambda (p)
;;                                        (if (instance? p)
;;                                            (name-sym (instance-name p))
;;                                            (e&gm p 'nodeID)))
;;                                      process+))
;;                             ))
;;                   (connect thisID metaID)
                   )
                  )))]
     [err (<err> err)]
     ))
  
  (define-production (conditional expr)
    (table gv-pt)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug gv (printf "conditional~n"))
        (gm meta 'nodeID)]
     [err (<err> err)]
     ))
  
  (define-production (choice expr)
    (table gv-pt)
    (relies-on expression process err)
    (matches
     [(struct choice (meta e process))
      (debug gv (printf "choice~n"))
      (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table gv-pt)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug gv (printf "loop/while~n"))
      (let ([thisID (gm meta 'nodeID)])
        (string-append
         ;;(connect thisID e)
         (connect thisID process)
         (<process> process)))]
     [err (<err> err)]
     ))
  
  (define-production (action expr)
    (table gv-pt)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (<assignment> s)]
     [(? input? s) (<input> s)]
     [(? output? s) (<output> s)]
     [err (<err> err)]
     ))
  
  (define-production (assignment expr)
    (table gv-pt)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp))
      (debug gv (printf "assignment~n"))
      (let ([thisID (gm meta 'nodeID)])
          (string-append
           (<variable> var)
           (connect thisID var)
           ;;(connect thisID exp)
           ))]
     [err (<err> err)]
     ))
  
  (define-production (input expr)
    (table gv-pt)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug gv (printf "input~n"))
      (let ([thisID (gm meta 'nodeID)])
        (string-append
         (<chan> chan)
         (connect thisID chan "chan")
         (connect thisID var "var")
         (<variable> var)))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table gv-pt)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan e))
      (debug gv (printf "output~n"))
      (let ([thisID (gm meta 'nodeID)])
        (string-append
         (<chan> chan)
         (connect thisID chan "chan")
         ;;(connect thisID e)
         ))]
     [err (<err> err)]
     ))
  
  (define-production (expression expr)
    (table gv-pt)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug gv (printf "monadic-expression~n"))
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (debug gv (printf "dyadic-expression~n"))
      (make-dyadic-expression meta rator (<expression> rand1) (<expression> rand2))]
     [(? literal? s ) (<literal> s) ]
     [(? variable? s) (<variable> s)]
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table gv-pt)
    (relies-on err)
    (matches
     [(struct literal (meta num))
      (debug gv (printf "literal~n"))
      (label (gm meta 'nodeID) (stxexpand expr))]
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table gv-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug gv (printf "variable~n"))
      ;;(label (gm meta 'nodeID) (stxexpand expr))
      (let ([nodeID (gm meta 'nodeID)])
        (record nodeID `((id ,(stxexpand expr))
                           (loc ,(gm meta 'workspace-location))))
      )
       ]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table gv-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug gv (printf "name~n"))
      (let ([nodeID (gm meta 'nodeID)])
        ;;(label (gm meta 'nodeID) (stxexpand expr))
        (record nodeID `((id ,(stxexpand expr))
                           (loc ,(gm meta 'workspace-location))))
      )]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table gv-pt)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug gv (printf "type~n"))
      (label (gm meta 'nodeID) (stxexpand expr))]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table gv-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug gv (printf "chan~n"))
      (let ([nodeID (gm meta 'nodeID)])
        (record nodeID `((id ,(stxexpand expr))
                           (loc ,(gm meta 'workspace-location))))
      )]
     [err (<err> err)]
     ))
  
  (define (stxexpand node)
    (syntax-e (e&gm node 'stx)))
  
  (define-production (formal expr)
    (table gv-pt)
    (relies-on name type err)
    (matches
     [(struct formal (meta fname type))
      (debug gv (printf "formal~n"))
      (let ([nodeID (gm meta 'nodeID)])
        (record nodeID `((,(stxexpand fname) ,(stxexpand type))
                         (loc ,(e&gm fname 'workspace-location))))
      )]
     [err (<err> err)]
     ))
  
  ;; Create the pass using the gv grammar and the production table
  ;; from this module.
  (define graphviz 
    (make-identity0-grammar gv-pt))
  
  )