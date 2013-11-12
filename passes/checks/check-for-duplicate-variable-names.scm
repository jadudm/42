(module check-for-duplicate-variable-names mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide check-for-duplicate-variable-names)
  
  (define dvn-production-table
    (copy-production-table identity0-production-table))
  
  ;; duplicate-found? :: list -> bool or struct
  ;; Takes a list of name structures and compares
  ;; all the symbol names for duplicates. In particular,
  ;; it uses the symbolic information stored in the syntax object
  ;; to do the comparisons. This is the only "safe" way to do it;
  ;; Currently, it is necessary because we already uniquely 
  ;; renamed variables. We could have done that *after* this pass,
  ;; but ... well, this is the more resilient solution anyway (it can
  ;; now go anywhere that we're in the grammar of identity0, I suspect.)
  (define (duplicate-found? ls)
    (cond
      [(null? ls) #f]
      [else
       (let ([the-name-struct (car ls)]
             [rest (cdr ls)])
         (let ([name (syntax-e (e&gm the-name-struct 'stx))])
           (debug dvn
             (printf "Is '~a' in ~a?~n" 
                     name (map (lambda (ns)
                                 (syntax-e
                                  (e&gm ns 'stx))) rest)))
           (if (not (member name 
                            (map (lambda (ns)
                                   (syntax-e (e&gm ns 'stx)))
                                 rest))) 
               (duplicate-found? rest)
               the-name-struct)))]))
  
  
  ;; Check all the formals in the definition of a proc.
  (define-production (definition expr)
    (table dvn-production-table)
    (relies-on name formal process  err)
    (matches
     [(struct definition (meta name formal* process))
      (debug dvn (printf "definition~n"))
      (let ([duplicate-result
             (duplicate-found? (map formal-name formal*))])
        (if duplicate-result
            (exn42 definition duplicate-variable-name 
                   (format 
                    (tab-append
                     "'~a' is declared more than once in the same process definition.~n")
                    (syntax-e (e&gm duplicate-result 'stx)))
                   (e&gm duplicate-result 'pstx) 
                   (gm meta 'stx)
                   )))  
      (make-definition meta 
                       (<name> name) 
                       (map <formal> formal*)
                       (<process> process))]
     [err (<err> err)]
     ))
  
  ;; Check all the variables in a declaration block.
  (define-production (declaration expr)
    (table dvn-production-table)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug dvn (printf "declaration~n"))
      (let ([duplicate-result
             (duplicate-found? (map car name-type+))])
        (if duplicate-result
            (let ([meta (node-meta duplicate-result)])
              (exn42 declaration duplicate-variable-name 
                     (format 
                      (tab-append
                       "'~a' is declared more than once in the same declaration.")
                      (syntax-e (gm meta 'stx)))
                     (gm meta 'stx)
                     (gm meta 'pstx)))))
      
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
  
  (define check-for-duplicate-variable-names (make-identity0-grammar dvn-production-table))
  )