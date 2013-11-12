(module show-assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity2.scm" "42"))
  
  ;; Provide the pass
  (provide show-assembly)
  
  ;; Copy an id1 production table
  (define show-assembly-pt
    (copy-production-table identity2-production-table))
  
  (define (extract-value obj)
    (cond
      [(variable? obj)
       (variable-sym obj)]
      [(literal? obj)
       (literal-num obj)]
      [(label? obj)
       (primary-v obj)]
      [(symbol? obj) obj]
      [(comment? obj) 
       (format "COMMENT: ~a" (primary-v obj))]
      [else (exn42 catch-all show-assembly
                   (internal-compiler-error
                    (format 
                     (tab-append
                      "Could not extract anything from '~a'~n")
                     obj))
                   (e&gm obj 'stx)
                   (e&gm obj 'pstx))]))
  
  (define (print-symbolic-info obj)
    (cond
      [(primary? obj)
       (debug show-assembly (printf "~a ~a~n" 
                                    (get-asm-sym obj) 
                                    (extract-value (primary-v obj))))]
      [(secondary? obj)
       (debug show-assembly (printf "~a~n" (get-asm-sym obj)))]
      [(variable? obj)
       (debug show-assembly
         (printf "~a [~a, ~a] ~n" 
                 (variable-sym obj)
                 (e&gm obj 'variable-locality)
                 (e&gm obj 'variable-usage)
                 ))]
      [else
       (printf "~a~n" obj)]))
  
  (define-production (process expr)
    (table show-assembly-pt)
    (relies-on action construction instance specification asm err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [(? assembly? s) 
      (map print-symbolic-info (lift-in-order (assembly-instruction* s)))
      (<asm> s)]
     [err (<err> err)]
     ))
  
  (define-production (err expr)
    (table show-assembly-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all show-assembly
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define show-assembly 
    (make-identity2-grammar show-assembly-pt))
  )