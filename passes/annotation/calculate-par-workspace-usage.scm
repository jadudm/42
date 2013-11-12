(module calculate-par-workspace-usage mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide calculate-par-workspace-usage)
  
  (define cpwu-pt
    (copy-production-table identity0-production-table))
 
  ;; Process workspace usage lookup table
  (define PWULT (void))
  
  (define-production (mod expr)
    (table cpwu-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug cpwu (printf "PAR WORKSPACE USAGE : mod~n"))
      ;; Extract the workspace-usage-table so everyone else
      ;; can enjoy its numbery goodness.
      (set! PWULT (gm meta 'workspace-usage-table))

      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  ;; The "call-graph" is the PWULT above; 
  ;; it is a hash table that looks like:
  #|
    prefix : (id 5)
    comms.time : (consume succ seq.delta prefix 12)
    id : (2)
    succ : (3)
    delta : (4)
    consume : (6)
    seq.delta : (2)
  |#
  ;; Meaning that each instance is a key, and we have a list
  ;; of numbers and keys on the RHS. If we encounter a symbol,
  ;; we need to recursively walk and sum what comes back.
  ;; 
  ;; Really, though, this call graph doesn't take into account
  ;; the fact that a SEQ is the largest of the subprocesses
  ;; under it, and a PAR is the sum of the subprocesses under it.
  ;; 
  ;; So, in the end, we probably, really, need to walk the AST.
  (define (walk-call-graph* sym)
    (hash-table-get PWULT sym))
  
  (define (walk-call-graph sym)
    (let ([ls (walk-call-graph* sym)])
      (letrec ([h
                (lambda (ls)
                  (cond
                    [(null? ls) 0]
                    [(number? (car ls))
                     (+ (car ls) (h (cdr ls)))]
                    [(symbol? (car ls))
                     (+ (h (walk-call-graph* (car ls)))
                        (h (cdr ls)))]
                    [else
                     'shitty]))])
        (h ls))))
  
                 
    
  (define (usage-pair-lookup p stx)
    (case (car p)
      [(call) (+ 4 (walk-call-graph (cdr p)))]
      [(int) 1]
      [(par) (hash-table-get PWULT (cdr p)
                             (lambda ()
                               (exn42 catch-all usage-pair-lookup
                                      (internal-compiler-error
                                       (format "No data for PAR construct: ~a~n" p))
                                      stx)
                               ))]
      [else 
       (exn42 catch-all usage-pair-lookup
              (internal-compiler-error
               (format "Didn't handle: ~a~n" p))
              stx)]))
  
  (define (usage-not-pair-lookup p stx)
    (case p
      [(output input) 3]
      [else
       (exn42 catch-all usage-not-pair-lookup
              (internal-compiler-error
               (format "Didn't handle: ~a~n" p))
              stx)]))
    
  (define (calc-usage ws stx)
    ;; HACK
    (+ 10 
       (apply + (map (lambda (p)
                       (if (pair? p)
                           (usage-pair-lookup p stx)
                           (usage-not-pair-lookup p stx)))
                     ws))))
           
  (define-production (par expr)
    (table cpwu-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (let* (;; Grab our workspace-usage for each branch.
             [workspace-usage+
              (gm meta 'workspace-usage)]
             ;; Turn each branch's usage into a number
             [branch-numerical-usage+
              (map (lambda (w)
                     (calc-usage w (gm meta 'stx)))
                   workspace-usage+)])
        (debug cpwu (printf "par~n"))
        ;; I think I'd like this data in the par usage table.
        ;; It will matter when we calculate the amount of space
        ;; a given definition requires.
        (debug cpwu 
          (printf "Inserting ~a into the PWULT for ~a~n"
                  (apply + branch-numerical-usage+) (gm meta 'nodeID)))
        
        (hash-table-put! PWULT (gm meta 'nodeID)
                         (apply + branch-numerical-usage+))
        (im! (make-par meta (map <process> process+))
             `((par-branch-workspace-usage
                ,branch-numerical-usage+)))
        )]
     [err (<err> err)]
     ))

  (define-production (seq expr)
    (table cpwu-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug cpwu (printf "seq~n"))
       (make-seq meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define calculate-par-workspace-usage (make-identity0-grammar cpwu-pt))
  
  )