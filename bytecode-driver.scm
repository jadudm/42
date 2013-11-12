(module bytecode-driver mzscheme
  (require 
   (lib "debug.scm" "42")
   (lib "passes/parsing/expand.scm" "42")
   ;; 'only' is probably not necessary.
   (only (lib "passes/parsing/parse.scm" "42")
         parse)
   (only (lib "passes/identities/identity0.scm" "42")
         identity0)
   (only (lib "passes/identities/identity1.scm" "42")
         identity1)
   (only (lib "passes/identities/identity2.scm" "42")
         identity2)
   
   (only "show-assembly.scm"
         show-assembly)
   
   (only (lib "passes/viz/graphviz.scm" "42")
         graphviz)
   
   (only (lib "passes/annotation/uniquely-name-variables.scm" "42")
         uniquely-name-variables)
   
   (only (lib "passes/checks/check-for-valid-types.scm" "42")
         check-for-valid-types)
   (only (lib "passes/checks/check-for-duplicate-variable-names.scm" "42")
         check-for-duplicate-variable-names)
   (only (lib "passes/checks/check-instances-exist.scm" "42")
         check-instances-exist)
   (only (lib "passes/checks/check-for-duplicate-definitions.scm" "42")
         check-for-duplicate-definitions)
   (only (lib "passes/checks/check-for-locality-tag.scm" "42")
         check-for-locality-tag)
   (only (lib "passes/checks/check-only-top-level-definitions.scm" "42")
         check-only-top-level-definitions)

   
   ;; New 20070427 
   (only (lib "passes/annotation/tag-workspace-usage.scm" "42")
         tag-workspace-usage)
   
   (only (lib "passes/annotation/tag-variable-workspace-locations.scm" "42")
         tag-variable-workspace-locations)
   
  (only (lib "passes/annotation/calculate-process-offsets.scm" "42")
         calculate-process-offsets)
   
   (only (lib "passes/annotation/calculate-process-workspace-usage.scm" "42")
         calculate-process-workspace-usage)
   
   (only (lib "passes/annotation/calculate-par-workspace-usage.scm" "42")
         calculate-par-workspace-usage)
   
   (only (lib "passes/annotation/retag-variables-with-par-offset.scm" "42")
         retag-variables-with-par-offset)
   
   ;; Two passes that are not well understood, and 
   ;; disabled for now.
   #;(only (lib "passes/checks/check-only-top-level-procs.scm" "42")
         check-only-top-level-procs)
   (only (lib "passes/annotation/assign-workspace-usage.scm" "42")
         assign-workspace-usage)

   (only (lib "passes/annotation/tag-variables-as-local-or-nonlocal.scm" "42")
         tag-variables-as-local-or-nonlocal)
   
   (only (lib "passes/asm/expressions2assembly.scm" "42")
         expressions2assembly)
   (only (lib "passes/asm/seq-par2assembly.scm" "42")
         seq-par2assembly)
   (only (lib "passes/asm/assignment2assembly.scm" "42")
         assignment2assembly)
   (only (lib "passes/asm/conditional2assembly.scm" "42")
         conditional2assembly)
   (only (lib "passes/asm/loop2assembly.scm" "42")
         loop2assembly)
   
   (only (lib "passes/asm/write-assembly.scm" "42")
         write-assembly)
   
   (lib "pretty.ss")
   )
  
  (provide run)
  
  (define tree 'boogers)
  
  ;; If we don't want our compiler to dump context
  ;; (which we don't, since it would be PLT Scheme
  ;; context, and not soccam context), we should
  ;; disable the printing of context.
  ;;(error-print-context-length 0)
  ;; Of course, we don't want to do this while developing...
  
  ;; For now, lets print everything... except the hash tables.
  (print-struct #t)
  ;;(print-hash-table #t)
  
  (define-syntax (run-once stx)
    (syntax-case stx ()
      [(_ name)
       #`(cons (quote name)
               (lambda (tree)
                 (debug driver (printf "Running ~a.~n" (quote name)))
                 (name tree))
               )]))
  
  (define (show-tree tree)
    (debug driver
      (printf "Compilation output:~n")
      (pretty-display tree)
      tree))
  
  (define drive-pass
    (lambda (pair)
      (if (pair? pair)
          (let ([start (current-milliseconds)]
                [pass-sym (car pair)]
                [pass (cdr pair)])
            (set! tree (pass tree))
            (debug profiler 
              (printf "~a : ~a~n" pass-sym (- (current-milliseconds) start)))
            ))))

  (define (run exp)
    (set! tree exp)
    (for-each drive-pass
              (list 
               (run-once expand42)
               (run-once parse)

               (disable identities
                        (run-once identity0))

               (run-once uniquely-name-variables)
               (disable identities
                        (run-once identity0))
               (disable syntax
                        (run-once check-for-valid-types))
               (disable identities
                        (run-once identity0))
               (disable syntax
                        (run-once check-for-duplicate-variable-names))
               (disable identities
                        (run-once identity0))
               (disable syntax
                        (run-once check-instances-exist))
               (disable identities
                        (run-once identity0))
               (disable syntax
                        (run-once check-for-duplicate-definitions))
               (disable identities
                        (run-once identity0))
               (disable syntax
                        (run-once check-only-top-level-definitions))
               (disable identities
                        (run-once identity0))               


               ;; The annotation passes to support graphviz are not
               ;; yet in this driver... so it shouldn't be run.
               ;;(run-once graphviz)
               ;;(disable identities (run-once identity0))
               
               (run-once assign-workspace-usage)
               (disable identities (run-once identity0))
   
               (run-once tag-variables-as-local-or-nonlocal)
               (disable identities (run-once identity0))

               ;;;;;;;;;;;;;;;  CALCULATION ;;;;;;;;;;;;;;;;;;;
               ;; These passes do some calculation --- which is 
               ;; probably wrong! --- to layout the workspace.
               ;; By "probably wrong" we mean "it works, but it works
               ;; because we were too generous, and are not generating
               ;; the right numbers and offsets for a tight workspace 
               ;; layout." At least, I think that's the case. (MCJ)
               
               (disable syntax (run-once tag-workspace-usage))
               (disable identities (run-once identity0))
               
               (run-once calculate-process-offsets)
               (disable identities (run-once identity0))
               
               (run-once tag-variable-workspace-locations)
               (disable identities (run-once identity0))
               
               (run-once retag-variables-with-par-offset)
               (disable identities (run-once identity0))
               
               ;;;;;;;;;;;;;;;;; ID1 ;;;;;;;;;;;;;;;;;;;;;;;;
               
               ;; This pass takes us into ID1
               (run-once expressions2assembly)
               (disable identities (run-once identity1))
               
               ;; This pass takes us into ID2. It would be 
               ;; Really nice to not name these "identityN".
               (run-once assignment2assembly)
               (disable identities (run-once identity2))
               
               (run-once conditional2assembly)
               (disable identities (run-once identity2))
               
               (run-once loop2assembly)
               (disable identities (run-once identity2))
               
               (run-once show-assembly)
               (disable identities (run-once identity2))
               
               ;; Not implemented
               ;;(run-once seq-par2assembly)
               ;;(disable identities (run-once identity2))
               
               ;; (run-once show-tree)
               (run-once write-assembly)
               
               )))
  )
