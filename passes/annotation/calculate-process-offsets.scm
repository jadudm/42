(module calculate-process-offsets mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide calculate-process-offsets)
  
  (define cpo-pt
    (copy-production-table identity0-production-table))
 
  (define-production (definition expr)
    (table cpo-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug tvwl (printf "definition~n"))
      ;; Assign workspace locations to all variables, channels, etc.
      (let* ([workspace-usage (gm meta 'workspace-usage)]
             [ws-types 
              (map (lambda (p)
                     (if (pair? p) (car p) p)) workspace-usage)]
             [ajw (calc-ajw-offset ws-types)]
             [process-size (calc-process-size ws-types)])
        
        (im! (make-definition meta 
                              (<name> name) 
                              (map <formal> formal*)
                              (<process> process))
             `((ajw-offset ,ajw)
               (process-size ,process-size))))]
     
     
     [err (<err> err)]
     ))
  
  
  (define (calc-process-size ws-types)
    (let ([total-size 0])
      (define (inc-size n) (set! total-size (+ total-size n)))
      
      ;; If the process contains input or output, we need
      ;; to allocate one additional space
      (if (or (member 'input ws-types)
              (member 'output ws-types))
          (inc-size 1))
      
      ;; If we have a call, inc by four
      (if (member 'call ws-types)
          (inc-size 4))
      
      ;; For each integer, increase the size by one.
      ;; Channels, at this point, as well.
      (for-each
       (lambda (t)
         (if (member t '(int chan))
             (inc-size 1))) ws-types)
      
      ;; For each PAR, add two for each control block.
      (for-each
       (lambda (t)
         (if (equal? t 'par)
             (inc-size 2))) ws-types)
      
      total-size))
  
  (define (calc-ajw-offset ws-types)
    (let ([total-size 0])
      (define (inc-size n) (set! total-size (+ total-size n)))
      
      ;; Do we have a PAR?
      (for-each
       (lambda (t)
         (if (member t '(par)) (inc-size 2)))
       ws-types)
      
      ;; For each integer in the process we're calling,
      ;; we'll need one slot. So, we'll need to adjust
      ;; to create space for them.
      (for-each
       (lambda (t)
         (if (member t '(int chan)) (inc-size 1)))
       ws-types)
      
      total-size
      ))
  
  (define calculate-process-offsets (make-identity0-grammar cpo-pt))
  
  )