(module tag-variables-as-local-or-nonlocal mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  (provide tag-variables-as-local-or-nonlocal)
  
  ;;OVERRIDES
  ;; definition
  ;; declaration
  ;;PURPOSE
  ;; Inserts metadata into variable structures as to whether
  ;; they are local or nonlocal. 
  
  (define env (make-hash-table))
  (define (add-name sym variable-locality)
    (hash-table-put! env sym variable-locality))
  
  (define tvalon-pt
    (copy-production-table identity0-production-table))
  
  
  (define-production (definition expr)
    (table tvalon-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug tvlnl (printf "definition~n"))
      
      ;; Tag the variables coming in as a formal to a proc
      ;; as 'nonlocal', as they didn't originate here...
      (foreach ([f formal*])
        (add-name (name-sym (formal-name f)) 'nonlocal))

      (make-definition meta 
                       (<name> name)
                       (map <formal> formal*)
                       (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table tvalon-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug tvlnl (printf "declaration~n"))
      ;; Tag the locally declared variables as 'local 
      (foreach ([nt name-type+])
        (add-name (name-sym (car nt)) 'local))
      
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
  
  (define-production (variable expr)
    (table tvalon-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug tvlnl (printf "variable~n"))
      (im! (make-variable meta var) 
           `((variable-locality ,(hash-table-get env var))))]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table tvalon-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug tvlnl (printf "name~n"))
      (im! (make-name meta n)
           `((variable-locality ,(hash-table-get env n (lambda () 'boogers)))))]
     [err (<err> err)]
     ))
  
  (define tag-variables-as-local-or-nonlocal
    (make-identity0-grammar tvalon-pt))
  )