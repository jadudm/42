(module calculate-process-workspace-usage mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide calculate-process-workspace-usage)
  
  (define cpwu-pt
    (copy-production-table identity0-production-table))
 
  ;; Process workspace usage lookup table
  (define PWULT (make-hash-table))
  (define (PWULT-cons sym val)
    (hash-table-put! PWULT 
                     sym
                     (cons 
                      val
                      (hash-table-get PWULT sym (lambda () '())))))

  ;; The ugly, stateful way. Saves passing data all the way 
  ;; down to the <instance>.
  (define *current-definition* (void))
  
  (define-production (mod expr)
    (table cpwu-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug cpwu (printf "mod~n"))
      (let ([process+ (map <process> process+)])
        (debug cpwu
          (hash-table-for-each
           PWULT
           (lambda (k v)
             (printf "~a : ~a~n" k v))))
        
        (im! (make-mod meta process+)
             `((workspace-usage-table ,PWULT))))]
     [err (<err> err)]
     ))
  
  (define-production (definition expr)
    (table cpwu-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug cpwu (printf "definition~n"))
      (let ([set-current-def
             (lambda () (set! *current-definition* (name-sym name)))])
      ;; Build up this process's size
      (PWULT-cons (name-sym name) 
                  (gm meta 'process-size))
      
        ;;Because we've opted for a stateful approach,
        ;; we have to use dynamic-wind to set and reset the 
        ;; 'current-definition' state. Kinda ugly, but again,
        ;; it's an experiment. This marks the first pass where
        ;; we really didn't take advantage of the treewalk.
        (make-definition meta 
                         (<name> name) 
                         (map <formal> formal*)
                         (dynamic-wind
                          set-current-def
                          (lambda () (<process> process))
                          set-current-def
                          )))]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table cpwu-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug cpwu (printf "instance~n"))
      ;; Indicate that this instance is enclosed in the current
      ;; definition.
      (PWULT-cons *current-definition* (name-sym name))
      (make-instance meta (<name> name) (map <name> actual*))]
     [err (<err> err)]
     ))
  
  (define calculate-process-workspace-usage (make-identity0-grammar cpwu-pt))
  
  )