(module tag-variable-workspace-locations mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide tag-variable-workspace-locations)
  
  (define tvwl-pt
    (copy-production-table identity0-production-table))
 
  ;; Variable Lookup Table
  (define VLT (make-hash-table))
  
  (define (adjust-start-for-par wsu)
    (let ([start 0])
      (for-each (lambda (w)
                  (if (and (pair? w)
                           (equal? 'par (car w)))
                      (set! start 2))) wsu)
      start))
        
  (define-production (definition expr)
    (table tvwl-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug tvwl (printf "definition~n"))
      ;; Assign workspace locations to all variables, channels, etc.
      (let* ([workspace-usage (gm meta 'workspace-usage)]
             ;; Start variables at location 0 or 2.
             [location (adjust-start-for-par workspace-usage)]
             [ajw-offset (gm meta 'ajw-offset)])
        
        (define (next-loc) 
          (let ([next location])
            (set! location (add1 location))
            next))
        
        ;; Assign locations to LOCAL variables and channels
        (for-each (lambda (pair)
                    (if (and (pair? pair)
                             (member (car pair) '(int chan)))
                        (hash-table-put! VLT (cdr pair) (next-loc))))
                  workspace-usage)
        
        ;; Assign locations to formals (NONLOCAL); 
        ;; restart at location one.
        (set! location 1)
        (for-each (lambda (pair)
                    (if (and (pair? pair)
                             (member (car pair) '(formal)))
                        (hash-table-put! VLT (cdr pair) (+ ajw-offset (next-loc)))))
                  workspace-usage)
        
        (make-definition meta 
                         (<name> name) 
                         (map <formal> formal*)
                         (<process> process))
        )]
     
     
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table tvwl-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug tvwl (printf "variable~n"))
      (im! (make-variable meta var)
           `((workspace-location ,(hash-table-get VLT var))))]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table tvwl-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug tvwl (printf "chan~n"))
      (im! (make-chan meta sym)
           `((workspace-location ,(hash-table-get VLT sym))))]
     [err (<err> err)]
     ))
  
  ;; Needed for instances
  (define-production (name expr)
    (table tvwl-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug tvwl (printf "name~n"))
      (im! (make-name meta n)
           ;; Silently die if we get an instance name here...
           `((workspace-location ,(hash-table-get VLT n (lambda () 'instance-name)))))]
     [err (<err> err)]
     ))
  
  (define tag-variable-workspace-locations (make-identity0-grammar tvwl-pt))
  
  )