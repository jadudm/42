(module efficienC-driver mzscheme
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


   (only (lib "passes/annotation/assign-workspace-usage.scm" "42")
         assign-workspace-usage)
   
   (only (lib "passes/annotation/tag-workspace-usage.scm" "42")
         tag-workspace-usage)
   
   ;; Two passes that are not well understood, and 
   ;; disabled for now.
   #;(only (lib "passes/checks/check-only-top-level-procs.scm" "42")
         check-only-top-level-procs)

   (only (lib "passes/annotation/tag-variable-workspace-locations.scm" "42")
         tag-variable-workspace-locations)
   
   (only (lib "passes/annotation/tag-variables-as-local-or-nonlocal.scm" "42")
         tag-variables-as-local-or-nonlocal)
   
   
   (only (lib "passes/annotation/calculate-process-offsets.scm" "42")
         calculate-process-offsets)
   
   (only (lib "passes/annotation/calculate-process-workspace-usage.scm" "42")
         calculate-process-workspace-usage)
   
   (only (lib "passes/annotation/calculate-par-workspace-usage.scm" "42")
         calculate-par-workspace-usage)
   
   (only (lib "passes/annotation/retag-variables-with-par-offset.scm" "42")
         retag-variables-with-par-offset)
   
   
   
   (only (lib "passes/native/native.scm" "42")
         native)
   
   (only (lib "passes/native/output-c.scm" "42")
         output-c)
   
   (only (lib "passes/viz/graphviz.scm" "42")
         graphviz)
   
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
  (print-hash-table #t)
  
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

  (define (print-c-program los)
    (printf "#include \"efficienC.h\"~n")
    (printf "void occam_program()~n{~n")
    (printf "goto PROCmain;~n")
    (for-each (lambda (str)
                (printf "~a" str))
              los)
    (printf "}~n"))
  
  (define (pass-through x) x)
  
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
               
               (run-once assign-workspace-usage)
               (disable identities (run-once identity0))
               
               (disable syntax
                        (run-once tag-workspace-usage))
               (disable identities
                        (run-once identity0))
               
               (run-once tag-variables-as-local-or-nonlocal)
               (disable identities (run-once identity0))
               
               (run-once calculate-process-offsets)
               (disable identities (run-once identity0))
               
               (run-once tag-variable-workspace-locations)
               (disable identities (run-once identity0))
               
               (run-once retag-variables-with-par-offset)
               (disable identities (run-once identity0))
               
               (run-once native)
               (disable identities (run-once identity0))
               
               ;; Big cut of commented out code from below goes here.
               
               
               (run-once output-c)
               
               
               (run-once print-c-program)
               )))
  #|
          ;; Disabled until the pass is completed, and better understood.
               (disable syntax
                        (run-once tag-workspace-usage))
               (disable identities
                        (run-once identity0))
               
               ;; tvalonl was naive, and doesn't have any meaning
               ;; in the real world. We'll use it for now,
               ;; because we are disallowing (proc ...) forms that
               ;; are not at the top level.
               (run-once tag-variables-as-local-or-nonlocal)

               (disable identities
                        (run-once identity0))
               
               ;; cflt checks that tvalon actually inserted the tag
               ;; in all struct:variables, and dies if it finds one
               ;; missing a 'variable-locality key in its metadata, and checks
               ;; that the value is either 'local' or 'nonlocal'
               (run-once check-for-locality-tag)
               (disable identities
                        (run-once identity0))
               
               (run-once calculate-process-offsets)
               (disable identities (run-once identity0))
               
               (run-once calculate-process-workspace-usage)
               (disable identities (run-once identity0))
               
               (run-once calculate-par-workspace-usage)
               (disable identities (run-once identity0))
               
               (run-once tag-variable-workspace-locations)
               (disable identities (run-once identity0))
               
               (run-once retag-variables-with-par-offset)
               (disable identities (run-once identity0))
               
               ;;(run-once show-tree)
               
               (if (debug-flag? 'graphviz)
                   (run-once graphviz)
                   pass-through)
               |#
  )