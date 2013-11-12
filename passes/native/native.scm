(module native mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42")
           (lib "pregexp.ss"))
  
  ;; We'll want to reuse this pass.
  (provide native)
  
  (define native-pt
    (copy-production-table identity0-production-table))
  
  (define-production (err expr)
    (table native-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all native
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define-production (mod expr)
    (table native-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug native (printf "mod~n"))
      (make-mod meta (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table native-pt)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (<action> s)]
     [(? specification? s) (<specification> s)]
     [(? construction? s) (<construction> s)]
     [(? instance? s) (<instance> s)]
     [err (<err> err)]
     ))
  
  (define-production (definition expr)
    (table native-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug native (printf "definition~n"))
      
      (im! (make-definition meta 
                            (<name> name) 
                            (map <formal> formal*)
                            (<process> process))
           ;; Workspace requirements also need to be known...
           `((c ,(format 
                  (string-append
                   "PROC~a:~n")
                  (name-sym name)
                  ))))]
     
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table native-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug native (printf "declaration~n"))
      (let ([chan-decl-strings
             (apply string-append
                    (map (lambda (nt)
                           (let ([name (car nt)]
                                 [type (cadr nt)])
                             (if (equal? (type-sym type) 'chan)
                                 (string-append
                                  (format 
                                   "wptr[~a] = /* ~a @~a */ NOT_PROCESS_P;~n"
                                   (e&gm name 'offset-workspace-location)
                                   (name-sym name)
                                   (e&gm name 'workspace-location))
                                  (format
                                   "debug(\"[~a @wsloc %x]\\n\", (wptr + ~a));~n"
                                   (name-sym name)
                                   (e&gm name 'workspace-location)))
                                   
                                 ""))) name-type+))])
        
        (im! (make-declaration meta 
                               (map 
                                (lambda (pair)
                                  (list 
                                   (<name> (car pair))
                                   (<type> (cadr pair))))
                                name-type+)
                               (<process> process))
             ;; These become a workspace allocation...
             
             `((c ,chan-decl-strings))))
      ]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table native-pt)
    (relies-on seq conditional loop par err)
    (matches
     [(? seq? s) (<seq> s)]
     [(? conditional? s) (<conditional> s)]
     ;; All loops have a parent loop structure
     [(? loop? s) (<loop> s)]
     [(? par? s) (<par> s)]
     [err (<err> err)]
     ))
  
  (define-production (specification expr)
    (table native-pt)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s) (<definition> s)]
     [err (<err> err)]
     ))
  
  (define (optional-ampersand o)
    (cond
      [(number? o) o]
      [else
       ;;(format "&~a" o)
       (let ([locality (list-ref (pregexp-match "\\(locality (.*?)\\)" o) 1)])
         (if (equal? locality "local")
             (pregexp-replace "wptr\\[(\\d+)\\]"
                              o "(wptr + \\1)")
             (pregexp-replace "wptr\\[(\\d+)\\]"
                              o "*(wptr + \\1)")))
       ]))
       
  (define-production (instance expr)
    (table native-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug native (printf "instance~n"))
      (let* ([namerec (<name> name)]
             [actualsrec (map <name> actual*)]
             [callreturn (gensym (format "return_from_~a" (name-sym name)))]
             [var-names #(xxx xxx xxx)]
             [c-args #(#xdeadbeef #xdeadbeef #xdeadbeef)])
        
        (for-each (let ([c -1])
                    (lambda (actual)
                      (set! c (add1 c))
                      (vector-set! c-args c 
                                   (format "wptr[~a] /* ~a @~a  (locality ~a) */"
                                           (e&gm actual 'offset-workspace-location)
                                           (name-sym actual)
                                           (e&gm actual 'workspace-location)
                                           (e&gm actual 'variable-locality)
                                           ))))
                  actualsrec)
        
        ;; For debugging
        (for-each (let ([c -1])
                    (lambda (actual)
                      (set! c (add1 c))
                      (vector-set! var-names c (name-sym actual))))
                  actualsrec)
        
        (im! (make-instance meta namerec actualsrec)
             `((c ,(format 
                    (string-append
                     "wptr[-4] = (int) &&~a;~n"
                     "wptr[-3] = ~a;~n"
                     "wptr[-2] = ~a;~n"
                     "wptr[-1] = ~a;~n"
                     (format 
                      "debug(\"call ~a: [-3 ~a %x] [-2 ~a %x] [-1 ~a %x]\\n\", wptr[-3], wptr[-2], wptr[-1]);~n"
                      (name-sym name)
                      (vector-ref var-names 0)
                      (vector-ref var-names 1)
                      (vector-ref var-names 2))
                     "wptr = wptr - 4;~n"
                     "goto PROC~a;~n"
                     "~a:~n")
                    callreturn
                    (optional-ampersand (vector-ref c-args 0))
                    (optional-ampersand (vector-ref c-args 1))
                    (optional-ampersand (vector-ref c-args 2))
                    (name-sym name)
                    callreturn))))
        )]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table native-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug native (printf "seq~n"))
      (im! (make-seq meta (map <process> process+))
           `((c "")))]
     [err (<err> err)]
     ))
  
  
  
  (define (add-to-queues par+rec parlabels par-branch-sizes)
    (apply string-append
           (cdr 
            (map (let ([sum 0])
                   (lambda (process label size)
                     (let ([str
                            (format "add_to_queue((int)(wptr - ~a), (int)&&~a);~n"
                                    sum
                                    label)])
                       (set! sum (+ sum size))
                       str)))
                 par+rec
                 parlabels
                 par-branch-sizes))))
  
  (define bits-of-par
    (lambda (process-strings) 
      (lambda (parlabels par-branch-sizes)
        (let ([last-size 0])
          (apply string-append
                 (map (lambda (process label size)
                        (let ([offset last-size])
                          (set! last-size (+ size last-size))
                          (format 
                           (string-append
                            "~a:~n"
                            "~a~n"
                            "goto *(endp((int) (wptr - ~a)));~n" ;; /* MAGICTIMESPROCESSNUMBER */
                            )
                           label
                           process
                           (+ size offset)
                           )))
                      process-strings parlabels par-branch-sizes))))))
  
  (define-production (par expr)
    (table native-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug native (printf "par~n"))
      (let* ([par+rec (map <process> process+)]
             [parlabels (map (lambda (p)
                               (gensym 'par)) par+rec)]
             [endparlab (gensym 'endparlab)]
             ;; Using the new 'assign-workspace-usage' data...
             [par-branch-sizes 
              (map (lambda (ls)
                     (apply + ls))
                   (gm meta 'workspace-size))])
        (debug native
          (printf "Par branch sizes: ~a~n" par-branch-sizes))
        
        (im! (make-par meta par+rec)
             `((c ,(lambda bop
                     (format 
                      (string-append
                       "~n/* PAR ~a - ~a START */~n"
                       "wptr[1] = ~a;~n"
                       "wptr[0] = &&~a;~n"
                       "~a"
                       "~a"
                       "~a:~n"
                       )
                      endparlab (length par+rec)
                      (length par+rec)
                      endparlab
                      (add-to-queues par+rec 
                                     parlabels
                                     par-branch-sizes)
                      ;; Cute! Higher-order functions!
                      ((apply bits-of-par bop) 
                       parlabels
                       par-branch-sizes)
                      
                      endparlab))))))]
     [err (<err> err)]
     ))
  
  (define-production (conditional expr)
    (table native-pt)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug native (printf "conditional~n"))
      (make-conditional meta (map <choice> choice+))]
     [err (<err> err)]
     ))
  
  (define-production (choice expr)
    (table native-pt)
    (relies-on expression process err)
    (matches
     [(struct choice (meta e process))
      (debug native (printf "choice~n"))
      (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table native-pt)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug native (printf "loop/while~n"))
      (let ([erec (<expression> e)]
            [prec (<process> process)])
        (im! (make-while meta erec prec)
             `((c ""))))]
     [err (<err> err)]
     ))
  
  (define-production (action expr)
    (table native-pt)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (<assignment> s)]
     [(? input? s) (<input> s)]
     [(? output? s) (<output> s)]
     [err (<err> err)]
     ))
  
  (define-production (assignment expr)
    (table native-pt)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp))
      (debug native (printf "assignment~n"))
      (let ([varrec (<variable> var)]
            [exprec (<expression> exp)])
        (im! (make-assignment meta
                              varrec
                              exprec)
             `((c ,(format "~a = ~a;~n" 
                           (e&gm varrec 'c)
                           (e&gm exprec 'c))))))]
     [err (<err> err)]
     ))
  
  (define-production (input expr)
    (table native-pt)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug native (printf "input~n"))
      (let ([jumpsym (gensym 'input_jump)]
            [chanrec (<chan> chan)]
            [varrec (<variable> var)])
        (im! (make-input meta chanrec varrec)
             `((c ,(format 
                    (string-append
                     (format "debug(\"[in '~a' l: ~a c: ~a]: %x\\n\", (int*)~a);~n"
                             (syntax-e (e&gm chan 'stx))
                             (syntax-line (e&gm chan 'stx))
                             (syntax-column (e&gm chan 'stx))
                             (e&gm chanrec 'c))
                     "goto *(in (4, (int *)~a, (BPOOTER)&~a, &&~a));~n~a:~n")
                    (e&gm chanrec 'c)
                    (e&gm varrec 'c)
                    jumpsym
                    jumpsym)))))]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table native-pt)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan e))
      (debug native (printf "output~n"))
      (let ([jumpsym (gensym 'output_jump)]
            [chanrec (<chan> chan)]
            [erec (<expression> e)])
        (im! (make-output meta chanrec erec)
             `((c ,(format 
                    (string-append
                     (format "debug(\"[out '~a' l: ~a c: ~a]: %x\\n\", (int*)~a);~n"
                             (syntax-e (e&gm chan 'stx))
                             (syntax-line (e&gm chan 'stx))
                             (syntax-column (e&gm chan 'stx))
                             (e&gm chanrec 'c))
                     "goto *(out (4, (int *)~a, (BPOOTER)&~a, &&~a));~n~a:~n")
                     ;; For the goto
                     (e&gm chanrec 'c)
                     (e&gm erec 'c)
                     jumpsym
                     jumpsym)))))]
     [err (<err> err)]
     ))
  
  (define-production (expression expr)
    (table native-pt)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug native (printf "monadic-expression~n"))
      
      (exn42 catch-all monadic-native
             (internal-compiler-error "Not implemented.")
             (gm meta 'stx)
             (gm meta 'pstx))
      
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (let ([rand1-rec (<expression> rand1)]
            [rand2-rec (<expression> rand2)])
        (debug native (printf "dyadic-expression~n"))
        (im! (make-dyadic-expression meta rator rand1-rec rand2-rec)
             `((c ,(format "(~a) ~a (~a)"
                           (e&gm rand1-rec 'c)
                           rator 
                           (e&gm rand2-rec 'c))))))]
     [(? literal? s ) (<literal> s)]
     [(? variable? s) (<variable> s)]
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table native-pt)
    (relies-on err)
    (matches
     [(struct literal (meta num))
      (debug native (printf "literal~n"))
      ;; THIS ONLY WORKS because we only have numbers and 'true' and 'false'
      ;; as literals right now.
      (im! (make-literal meta num)
           `((c ,(if (number? num)
                     num
                     (if (equal? num 'true)
                         1
                         0))
                         )))]
     [err (<err> err)]
     ))
  
  (define-production (variable expr)
    (table native-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug native (printf "variable~n"))
      (im! (make-variable meta var)
           `((c ,(format "wptr[~a] /* ~a @~a*/" 
                         (gm meta 'offset-workspace-location)
                         var
                         (gm meta 'workspace-location)
                         ))))]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table native-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug native (printf "name~n"))
      (make-name meta n)]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table native-pt)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug native (printf "type~n"))
      (make-type meta t)]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table native-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug native (printf "chan~n"))
      (im! (make-chan meta sym)
           `((c ,(format "wptr[~a] /* ~a @~a */" 
                         (gm meta 'offset-workspace-location)
                         sym
                         (gm meta 'workspace-location)
                         ))))]
     [err (<err> err)]
     ))
  
  (define-production (formal expr)
    (table native-pt)
    (relies-on name type err)
    (matches
     [(struct formal (meta name type))
      (debug native (printf "formal~n"))
      (make-formal meta (<name> name) (<type> type))]
     [err (<err> err)]
     ))
  
  
  ;; Create the pass using the native grammar and the production table
  ;; from this module.
  (define native
    (make-identity0-grammar native-pt))
  
  )