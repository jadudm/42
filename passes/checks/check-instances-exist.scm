(module check-instances-exist mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide check-instances-exist)
  
  (define cie-production-table
    (copy-production-table identity0-production-table))
  
  ;; Contains the list of defined proc names
  (define proc-names `())
  
  ;; add-proc-name :: name-structure -> void
  ;; Adds a proc name to the list of names when 
  ;; found.
  (define (add-proc-name name)
    (debug cie (printf "adding name: ~a ~a~n" name (name-sym name)))
    (set! proc-names (cons name proc-names)))
  
  ;; proc-name-defined? :: name-structure -> bool
  ;; Checks if a proc name (called from an instance)
  ;; is defined.  
  (define (proc-name-defined? name)
    (debug cie (printf "checking name: ~a ~a~n" name (name-sym name)))
    (member (name-sym name) (map name-sym proc-names)))
  
  ;;
  ;; MOD
  ;;
  (define-production (mod expr)
    (table cie-production-table)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug cie (printf "mod~n"))
      ;;Add all the proc names to a list
      (for-each (lambda (o)
                  (if (definition? o) 
                      (add-proc-name (definition-name o))))
                process+)
      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  ;;
  ;; INSTANCE
  ;;
  (define-production (instance expr)
    (table cie-production-table)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug cie (printf "instance~n"))
      (if (proc-name-defined? name)
          (make-instance meta (<name> name) (map <name> actual*))
          (exn42 instance not-defined
                 (format
                  (tab-append
                   "'~a' is not defined. For it to be defined~n"
                   "you must have something that looks like~n"
                   "~n"
                   "\t (proc ~a (...) ...)~n"
                   "~n"
                   "in your code, where the '...'s might be filled~n"
                   "in with code you write yourself.")
                  (name-sym name)
                  (name-sym name))
                 (gm meta 'stx)
                 (gm meta 'pstx)))]
     [err (<err> err)]
     ))
  
  (define check-instances-exist (make-identity0-grammar cie-production-table))
  
  )