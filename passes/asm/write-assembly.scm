(module write-assembly mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity2.scm" "42"))
  (require (lib "pretty.ss"))
  
  ;; Provide the pass
  (provide write-assembly)
  
  ;; Copy an id1 production table
  (define wa-pt
    (copy-production-table identity2-production-table))
  
  (define global-workspace-size 0)    
  (define last-definition 'poo)
  
  (define (flatten-asm ls)
    (cond
      [(null? ls) '()]
      [(symbol? (caar ls))
       (cons (car ls)
             (flatten-asm (cdr ls)))]
      [else
       (append (flatten-asm (car ls))
               (flatten-asm (cdr ls)))]
      ))
                          
  
  (define-production (mod expr)
    (table wa-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug wa (printf "mod~n"))
      (let ([asm (map <process> process+)])
        (let ([op (open-output-file "out.asm42" 'replace)])
          (parameterize ([current-output-port op])
            (pretty-print
             `(tvm
               (ws ,global-workspace-size)
               (vs 0)
               (ms 0)
               (asm
                ,@(flatten-asm asm)))))
          (close-output-port op))
        )]
     [err (<err> err)]
     ))
  
    (define-production (declaration expr)
    (table wa-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug wa (printf "deleting declaration~n"))
      (<process> process)]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table wa-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug wa (printf "inlining seq~n"))
      (map <process> process+)]
     [err (<err> err)]
     ))
  
  (define-production (definition expr)
    (table wa-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta def-name formal* process))
      (debug wa (printf "definition~n"))
      ;; Add up our workspace needs
      (let ([ws-size
             (+ (apply + (e&gm expr 'workspace-size))
                (e&gm expr 'process-size))]
	    ;; 20070428 MCJ
	    ;; Again, I need to think about this just a touch;
	    ;; I suspect that the offset should be negated, that is,
	    ;; move the pointer up by the offset. However, this
	    ;; will depend on how we ultimately decide to lay out
	    ;; memory w/ 42.
            [ajw-val (gm meta 'ajw-offset)]
            [name-symbol
             (name-sym def-name)])
        
        (set! global-workspace-size (+ global-workspace-size ws-size))
        
        ;; Last definition should work
        (set! last-definition name-symbol)
        
        `((jentry ,name-symbol)
          (procentry ,name-symbol)
          (ajw ,(* -1 ajw-val))
          ,@(<process> process)
          (ajw ,ajw-val)
	  (ret)))
      ]
     [err (<err> err)]
     ))
  
  
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
      [else (exn42 catch-all write-assembly
                   (internal-compiler-error
                    (format 
                     (tab-append
                      "Could not extract anything from '~a'~n")
                     obj))
                   (e&gm obj 'stx)
                   (e&gm obj 'pstx))]))
  
  (define (return-symbolic-info obj)
    (cond
      [(primary? obj)
       `(,(get-asm-sym obj) 
          ,(extract-value (primary-v obj)))]
      [(secondary? obj)
       `(,(get-asm-sym obj))]
      [else
       (exn42 catch-all write-assembly
              (format
               "You tried to return symbolic info from an '~a'~n"
               obj)
              (e&gm obj 'stx)
              (e&gm obj 'pstx))]))
              
  
  (define-production (asm expr)
    (table wa-pt)
    (relies-on process literal variable err)
    (matches
     [(? process? s) 
      (debug wa (printf "Process in ASM.~n"))
      (<process> s)]
     [(struct assembly (meta instructions+))
      (map <asm> instructions+)]
     
     [(? primary? expr)
      (debug wa (printf "primary~n")) 
      (return-symbolic-info expr)]
     
     [(? secondary? expr) 
      (debug wa (printf "secondary~n"))
      (return-symbolic-info expr)]
     
     [(? variable? v) 
      (exn42 catch-all write-assembly
             (format
              (tab-append 
               "There shouldn't be variables in the write-assembly ASM pass."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))
      ]
     [err (<err> err)]
     ))  
  
  
  (define write-assembly (make-identity2-grammar wa-pt)))
