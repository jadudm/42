(module retag-variables-with-par-offset mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide retag-variables-with-par-offset)
  
  (define rvwpo-pt
    (copy-production-table identity0-production-table))
 
  (define *par-offset* 0)
  (define *last-offset* 0)
  
 (define-production (par expr)
    (table rvwpo-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug rvwpo (printf "par~n"))
      (let* ([last-size 0]
             [process+ (map (lambda (p ws)
                              (let ([offset *last-offset*])
                                (set! *last-offset* (+ ws *last-offset*))
                                (dynamic-wind
                                 (lambda () (set! *par-offset* offset)) 
                                 (lambda ()
                                   (debug rvwpo
                                     (printf "~n--------~n")
                                     (printf "par-offset: ~a~n" *par-offset*)
                                     (printf "last-offset: ~a~n" *last-offset*)
                                     (printf "ws: ~a~n" ws))
                                   (<process> p))
                                 (lambda () (set! *par-offset* *last-offset*))
                                 )))
                            process+ (map (lambda (ls)
                                            (apply + ls))
                                          (gm meta 'workspace-size)))])
        (debug rvwpo
          (printf "Usage: ~a~n" (gm meta 'workspace-size)))
        
        (set! *par-offset* 0)
        (set! *last-offset* 0)
        (make-par meta process+))]
     [err (<err> err)]
     ))
  
  (define (offset-location meta)
    (debug rvwpo
      (printf "~n--------~n")
      (printf "workspace-location ~a~n" 
              (gm meta 'workspace-location))
      (printf "par-offset: ~a~n" *par-offset*)
      (printf "-------~n"))
    
    (let ([v (+ (gm meta 'workspace-location) *par-offset*)])
      (if (< v 0)
          (* -1 v)
          v)))
  
  (define-production (variable expr)
    (table rvwpo-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug rvwpo (printf "variable~n"))
      (debug rvwpo
        (printf "Changing ~a ~a offset ~a -> ~a (par offset ~a)~n"
                "variable"
                var
                (gm meta 'workspace-location)
                (offset-location meta)
                *par-offset*
                ))
      (im! (make-variable meta var)
           `((offset-workspace-location
              ,(offset-location meta))))]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table rvwpo-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug rvwpo (printf "chan~n"))
      (debug rvwpo
        (printf "Changing ~a ~a offset ~a -> ~a~n"
                "chan"
                sym
                (gm meta 'workspace-location)
                (offset-location meta)))
      (im! (make-chan meta sym)
           `((offset-workspace-location
              ,(offset-location meta))))]
     [err (<err> err)]
     ))
  
  ;; Needed for instances
  (define-production (name expr)
    (table rvwpo-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug rvwpo (printf "name~n"))
      (if (not (equal? 'instance-name (gm meta 'workspace-location)))
          (begin
            (debug rvwpo
              (printf "Changing ~a ~a offset ~a -> ~a (par offset ~a)~n"
                      "name"
                      n
                      (gm meta 'workspace-location)
                      (offset-location meta)
                      *par-offset*
                      ))
            (im! (make-name meta n)
                 ;; Silently die if we get an instance name here...
                 `((offset-workspace-location
                    ,(offset-location meta)))))
          (make-name meta n))]
     [err (<err> err)]
     ))
  
  (define retag-variables-with-par-offset (make-identity0-grammar rvwpo-pt))
  
  )