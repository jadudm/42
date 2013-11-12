(module check-for-duplicate-definitions mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42")
           (lib "list.ss"))
  
  (provide check-for-duplicate-definitions)
  
  (define cdd-production-table
    (copy-production-table identity0-production-table))
  
  ;; find-duplicate-proc-names :: list -> #f or struct name 
  ;; takes a list of definition structs and checks for duplicate
  ;; names.  If one is found it is returned, otherwise false is returned.
  (define (find-duplicate-proc-names lst)
    (cond
      [(null? lst) #f]
      ;; Argh!  Duplicate found
      [(member (name-sym (car lst)) (map name-sym (cdr lst))) 
       (debug cdd (printf "found duplicate ~a~n" (name-sym (car lst))))
       (car lst)]
      [else 
       (debug cdd (printf "checked name ~a~n" (name-sym (car lst))))
       (find-duplicate-proc-names (cdr lst))]))
  
  (define-production (mod expr)
    (table cdd-production-table)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug cdd (printf "mod~n"))
      (let* ([definitions (map definition-name 
                   (filter definition? process+))] 
             [duplicate-name-found 
             (find-duplicate-proc-names definitions)])
        (if duplicate-name-found
            ;; error - duplicate found
            (let ([duplicates (filter name? 
                            (map (lambda (element)
                                   (if (equal? 
                                        (name-sym duplicate-name-found) 
                                        (name-sym element))
                                       element)) 
                                 definitions))])
              (debug cdd (printf "duplicates are ~a~n" duplicates))
            (exn42 definition duplicate-proc-name 
                   (format 
                    (apply tab-append
                     `("The proc '~a' has been defined multiple times.~n"
                       "It has been defined in the following places:~n"
                       ,@(map (lambda (d)
                                (let ([stx (e&gm d 'stx)])
                                (format "\tLine: ~a, Column: ~a~n" 
                                        (syntax-line stx) 
                                        (syntax-column stx)))) 
                                duplicates)))
                    (name-sym duplicate-name-found))
                   (e&gm duplicate-name-found 'stx)
                   (e&gm duplicate-name-found 'pstx)) 
            )))
      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))

  (define check-for-duplicate-definitions (make-identity0-grammar cdd-production-table))
  )