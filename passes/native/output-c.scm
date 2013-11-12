(module output-c mzscheme
   (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42")
           (lib "pregexp.ss"))
  
  ;; We'll want to reuse this pass.
  (provide output-c)
  
  (define output-c-pt
    (copy-production-table identity0-production-table))
  
  (define-production (err expr)
    (table output-c-pt)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all output-c
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  (define (strip-dot o)
    (pregexp-replace* "\\." (format "~a" o) "_"))
  
  (define-production (mod expr)
    (table output-c-pt)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug output-c (printf "mod~n"))
      ;; Just want the strings back
      (map strip-dot (map <process> process+))
      ]
     [err (<err> err)]
     ))
  
  (define-production (process expr)
    (table output-c-pt)
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
    (table output-c-pt)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug output-c (printf "definition~n"))
      (string-append
       (format "~n/* PROC ~a */~n" (name-sym name))
       (gm meta 'c)
       (format "wptr = wptr - ~a;~n" (gm meta 'ajw-offset))
       (<process> process)
       (format "wptr = wptr + ~a;~n" (gm meta 'ajw-offset))
       (format "wptr = wptr + 4;~n")
       (format "goto *wptr[-4];~n")
       )]
     [err (<err> err)]
     ))
  
  (define-production (declaration expr)
    (table output-c-pt)
    (relies-on name type process err)
    (matches
     [(struct declaration (meta name-type+ process))
      (debug output-c (printf "declaration~n"))
      (string-append
       (gm meta 'c)
       (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (construction expr)
    (table output-c-pt)
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
    (table output-c-pt)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s)]
     [(? definition? s) (<definition> s)]
     [err (<err> err)]
     ))
  
  (define-production (instance expr)
    (table output-c-pt)
    (relies-on name err)
    (matches 
     [(struct instance (meta name actual*))
      (debug output-c (printf "instance~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  (define-production (seq expr)
    (table output-c-pt)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug output-c (printf "seq~n"))
      (apply string-append (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (par expr)
    (table output-c-pt)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug output-c (printf "par~n"))
      ((gm meta 'c) (map <process> process+))]
     [err (<err> err)]
     ))
  
  (define-production (conditional expr)
    (table output-c-pt)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug output-c (printf "conditional~n"))
      (make-conditional meta (map <choice> choice+))]
     [err (<err> err)]
     ))
  
  (define-production (choice expr)
    (table output-c-pt)
    (relies-on expression process err)
    (matches
     [(struct choice (meta e process))
      (debug output-c (printf "choice~n"))
      (make-choice meta (<expression> e) (<process> process))]
     [err (<err> err)]
     ))
  
  (define-production (loop expr)
    (table output-c-pt)
    (relies-on expression process err)
    (matches
     [(struct while (meta e process))
      (debug output-c (printf "loop/while~n"))
      (let ([if-top (gensym 'if_top)])
        (format 
         (string-append
          "~a:~n"
          "if (~a)~n{~n"
          "~a~n"
          "goto ~a;~n"
          "}~n")
         if-top
         (<expression> e)
         (<process> process)
         if-top
         ))]
     [err (<err> err)]
     ))
  
  (define-production (action expr)
    (table output-c-pt)
    (relies-on assignment input output err)
    (matches
     [(? assignment? s) (<assignment> s)]
     [(? input? s) (<input> s)]
     [(? output? s) (<output> s)]
     [err (<err> err)]
     ))
  
  (define-production (assignment expr)
    (table output-c-pt)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp))
      (debug output-c (printf "assignment~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  (define-production (input expr)
    (table output-c-pt)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug output-c (printf "input~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  (define-production (output expr)
    (table output-c-pt)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan e))
      (debug output-c (printf "output~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  (define-production (expression expr)
    (table output-c-pt)
    (relies-on literal variable err)
    (matches 
     [(struct monadic-expression (meta rator rand))
      (debug output-c (printf "monadic-expression~n"))
      
      (exn42 catch-all monadic-output-c
             (internal-compiler-error "Not implemented.")
             (gm meta 'stx)
             (gm meta 'pstx))
      
      (make-monadic-expression meta rator (<expression> rand))]
     [(struct dyadic-expression (meta rator rand1 rand2))
      (gm meta 'c)]
     [(? literal? s ) (<literal> s)]
     [(? variable? s) (<variable> s)]
     [err (<err> err)]
     ))
  
  (define-production (literal expr)
    (table output-c-pt)
    (relies-on err)
    (matches
     [(struct literal (meta num))
      (debug output-c (printf "literal~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  
  
  (define-production (variable expr)
    (table output-c-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (debug output-c (printf "variable~n"))
      (strip-dot (gm meta 'c))]
     [err (<err> err)]
     ))
  
  (define-production (name expr)
    (table output-c-pt)
    (relies-on err)
    (matches
     [(struct name (meta n))
      (debug output-c (printf "name~n"))
      (make-name meta n)]
     [err (<err> err)]
     ))
  
  (define-production (type expr)
    (table output-c-pt)
    (relies-on err)
    (matches
     [(struct type (meta t))
      (debug output-c (printf "type~n"))
      (make-type meta t)]
     [err (<err> err)]
     ))
  
  (define-production (chan expr)
    (table output-c-pt)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug output-c (printf "chan~n"))
      (gm meta 'c)]
     [err (<err> err)]
     ))
  
  (define-production (formal expr)
    (table output-c-pt)
    (relies-on name type err)
    (matches
     [(struct formal (meta name type))
      (debug output-c (printf "formal~n"))
      (make-formal meta (<name> name) (<type> type))]
     [err (<err> err)]
     ))

  
  ;; Create the pass using the output-c grammar and the production table
  ;; from this module.
  (define output-c
    (make-identity0-grammar output-c-pt))
  )