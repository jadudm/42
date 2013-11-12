(module parse mzscheme
  (require (lib "plt-match.ss")
           (lib "util/standard-includes.scm" "42")
           (lib "passes/parsing/parse-errors.scm" "42")
           (lib "passes/parsing/parse-helpers.scm" "42")) 
  
  (provide parse)
  
  (define parse-production-table
    (copy-production-table empty-production-table))
  
  ;;
  ;; MODULE
  ;;
  (define-production (mod expr)
    (table parse-production-table)
    (relies-on process)
    (matches
     [`(,(? syntax? stx)
         ,(? (ssym? 'module) m)
         ,file 
         ;; This should be the symbol 'soccam,
         ;; but I'm going to make it driver flags for now.
         ;;,(? (ssym? 'soccam) lang)
         ,lang-or-flags
         ,process+ ..1)
       ;; Add the flags
       (if (list? lang-or-flags)
           (let ([flag* (cdr lang-or-flags)])
             (foreach ([flag flag*])
               #;(printf "Adding debug flag: ~a~n" 
                         (syntax-object->datum flag))
               (add-debug-flag! (syntax-e flag)))))

       (im! (make-mod (empty-meta) (map (lambda (p)
                                     (<process> p stx))
                                   process+))
            `([nodeID ,(gensym 'mod)]
              [stx ,stx]
              [pstx #f]))]
     [err (error (format "~a" err))]))
  
  ;;
  ;; PROCESS
  ;;
  (define-production (process expr pstx)
    (table parse-production-table)
    (relies-on action construction instance specification err)
    (matches
     [`(,(? syntax? stx) 
         ,(? (ssym? 'stop)))
       ;; insert-meta! takes a structure and a list
       ;; of elements to add to the metadata, returning
       ;; the structure that has been mutated. This is a bit
       ;; redunant-redundant, but it makes it work in this framework
       (im! (make-stop (empty-meta)) 
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'stop)]))]
     [`(,(? syntax? stx) 
         ,(? (ssym? 'skip)))
       (im! (make-skip (empty-meta)) 
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'skip)]))]
     
     ;; These predicates have been modified to look inside syntax objects.
     ;; This is somewhat irreversible. Hence there is little change
     ;; to this code, but it expects that 'e' will be a syntax object of some sort...
     ;; or a list of syntax objects, and so on.
     [(? sexp:action? e) (<action> e pstx)]
     [(? sexp:construction? e) (<construction> e pstx)]
     [(? sexp:specification? e) (<specification> e pstx)]
     [(? sexp:inst? e) (<instance> e pstx)]
     [err (<err> err pstx)]
     ))
  
  (define-production (instance expr pstx)
    (table parse-production-table)
    (relies-on name err)
    (matches
     [`(,(? syntax? stx) ,proc-name ,actual-names* ...)
       ;; WARNING: This currently only takes declared variables
       ;; WARNING: This really means it will *not* take literals (eg. '3')
       ;; Note that <name> will properly handle the syntax information for
       ;; the symbolic names.
       (im! (make-instance (empty-meta)
                           (<name> proc-name stx)
                           (map (lambda (a)
                                  (<name> a stx))
                                actual-names*))
            `([stx ,stx] 
              [pstx ,pstx] 
              [nodeID ,(gensym 'inst)]))]
     ;; WARNING
     ;; Not sure what is going on here... I don't
     ;; think this has been exercised yet.
     [err (<err> expr pstx)]
     ))
  
  (define-production (specification expr pstx)
    (table parse-production-table)
    (relies-on declaration definition err)
    (matches
     [(? (first-sym? 'decl) e) 
      (debug specifcation
        (printf "Is a specification/declaration: ~a~n" e))
      (<declaration> e pstx)]
     [(? (first-sym? 'proc) e) (<definition> e pstx)]
     [err (<err> err pstx)]
     ))
  
  (define-production (declaration expr pstx)
    (table parse-production-table)
    (relies-on  name type process err)
    (matches
     ;; Hm. This matches an empty proc... not sure why that is.
     ;; 20060613 MCJ
     ;; Also, isn't (decl () ...) perfectly valid? 
     ;; I'm going to change ..1 to ... in the pattern, and 
     ;; name+/type+ into name*/type* to reflect this.
     [`(,(? syntax? stx)
         ,(? (ssym? 'decl))
         (,(? syntax? ig1)
           [,(? syntax? ig2+) ,name* ,type*] ...) 
         ,proc)
       (let ([pairs (map (lambda (n t)
                           ;; The names and types could inherit a
                           ;; different parent syntax, I suppose.
                           (list (<name> n stx) 
                                 (<type> t stx))) name* type*)])
         (im! (make-declaration (empty-meta) pairs (<process> proc stx))
              `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'decl)])))]
     ;; Ooh... if I want to use the case-based error 
     ;; handler above, I would use this line:
     ;; [err (<err> 'declaration expr pstx)]
     ;; But if I want to use the mixin-based handler,
     ;; where I mix in a new unit with the same signature
     ;; as 'err', I would instead use:
     [err 
      (debug declaration
        (printf "Calling declaration error.~n"))
      (<err> err pstx)]
     ))
  
  (define-production (definition expr pstx)
    (table parse-production-table)
    (relies-on name formal process err)
    (matches
     [`(,(? syntax stx)
         ,(? (ssym? 'proc))
         ,name (,(? syntax? fstx)
                 ,id* ...)
         ,body)
       (im! (make-definition (empty-meta)
                             (<name> name stx)
                             (map (lambda (f)
                                    (<formal> f stx)) id*)
                             (<process> body stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'def)]))]
     [err (<err> err pstx)]
     ))
  
  
  
  
  (define-production (action expr pstx)
    (table parse-production-table)
    (relies-on assignment input output err)
    (matches
     [(? (first-sym? ':=) e) (<assignment> e pstx)]
     [(? (first-sym? '?) e) (<input> e pstx)]
     [(? (first-sym? '!) e) (<output> e pstx)]
     [err (<err> err pstx)]
     ))
  
  (define-production (construction expr pstx)
    (table parse-production-table)
    (relies-on seq conditional loop par err)
    (matches
     [(? (first-sym? 'seq) e) (<seq> e pstx)]
     [(? (first-sym? 'par) e) (<par> e pstx)]
     ;; WARNING
     ;; Hacked if/cond
     [(? (first-sym? 'if) e) (<conditional> e pstx)]
     [(? (first-sym? 'cond) e) (<conditional> e pstx)]
     [(? (first-sym? 'while) e) (<loop> e pstx)]
     [err (<err> err pstx)]
     ))
  
  (define-production (seq expr pstx)
    (table parse-production-table)
    (relies-on process err)
    (matches
     ;; WARNING
     ;; Will need to handle replicators at some point
     ;; WARNING
     ;; Need to check that these are all processes before
     ;; recurring on them...
     [`(,(? syntax? stx)
         ,(? (ssym? 'seq))
         ,process+ ..1)
       (im! (make-seq (empty-meta)
                      (map 
                       (lambda (p) (<process> p stx))
                       process+))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'seq)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (par expr pstx)
    (table parse-production-table)
    (relies-on process err)
    (matches
     ;; WARNING
     ;; Will need to handle replicators at some point
     ;; WARNING
     ;; Need to check that these are all processes before
     ;; recurring on them...
     [`(,(? syntax? stx)
         ,(? (ssym? 'par))
         ,process+ ..1)
       (im! (make-par (empty-meta)
                      (map 
                       (lambda (p) (<process> p stx)) 
                       process+))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'par)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (loop expr pstx)
    (table parse-production-table)
    (relies-on expression process err)
    (matches
     ;; WARNING
     ;; Should check these are of the right type in the match
     [`(,(? syntax? stx)
         ,(? (ssym? 'while))
         ,e ,p)
       (im! (make-while (empty-meta)
                        (<expression> e stx) (<process> p stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'while)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (conditional expr pstx)
    (table parse-production-table)
    (relies-on choice err)
    (matches
     ;; WARNING 
     ;; Hacked 'if'
     [`(,(? syntax? stx)
         ,(? (ssym? 'if) ifsym)
         ,test ,true ,else)
       (<conditional> `(,stx 
                         ,(datum->syntax-object stx 'cond)
                         (,stx ,test ,true)
                         (,stx ,(datum->syntax-object stx 'true) ,else))
                      pstx)]
     ;; WARNING 
     ;; Hacked 'cond'
     [`(,(? syntax? stx)
         ,(? (ssym? 'cond))
         ,choice+ ..1)
       (im! (make-conditional (empty-meta)
                              (map (lambda (c)
                                     (<choice> c stx)) choice+))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'cond)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (choice expr pstx)
    (table parse-production-table)
    (relies-on expression process err)
    (matches
     [`(,(? syntax? stx) ,e ,p)
       (im! (make-choice (empty-meta)
                         (<expression> e stx) (<process> p stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'choice)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (assignment expr pstx) 
    (table parse-production-table)
    (relies-on variable expression err)
    (matches
     [`(,(? syntax? stx)
         ,(? (ssym? ':=))
         ,v ,e) 
       (im! (make-assignment (empty-meta)
                             (<variable> v stx)
                             (<expression> e stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'assign)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (input expr pstx)
    (table parse-production-table)
    (relies-on chan variable err)
    (matches 
     [`(,(? syntax? stx)
         ,(? (ssym? '?))
         ,ch ,v)
       (im! (make-input (empty-meta)
                        (<chan> ch stx) (<variable> v stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'in)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (output expr pstx)
    (table parse-production-table)
    (relies-on chan expression err)
    (matches
     [`(,(? syntax? stx)
         ,(? (ssym? '!))
         ,ch ,e)
       (im! (make-output (empty-meta)
                         (<chan> ch stx) (<expression> e stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'out)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (variable expr pstx)
    (table parse-production-table)
    (relies-on err)
    (matches
     ;; Should probably check that it is a symbol as well...
     ;; AHA!
     ;; (identifier? v) returns #t if v is a syntax object 
     ;;                 and (syntax-e stx) produces a symbol.
     [(? identifier? e) 
      (im! (make-variable (empty-meta) (syntax-object->datum e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'var)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (chan expr pstx)
    (table parse-production-table)
    (relies-on err)
    (matches
     [(? identifier? e) 
      (im! (make-chan (empty-meta) (syntax-object->datum e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'chan)]))]
     [err (<err> err pstx)]
     ))
  
  ;; Rator == operator
  ;; Rand == operand
  (define-production (expression expr pstx)
    (table parse-production-table)
    (relies-on literal variable err)
    (matches
     [`(,(? syntax? stx)
         ,(? sexp:monadic? rator) 
         ,rand)
       (im! (make-monadic-expression (empty-meta)
                                     (syntax-e rator)
                                     (<expression> rand stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'unexp)]))]
     [`(,(? syntax? stx)
         ,(? sexp:dyadic? rator)
         ,rand1 ,rand2)
       (im! (make-dyadic-expression (empty-meta)
                                    (syntax-e rator)
                                    (<expression> rand1 stx)
                                    (<expression> rand2 stx))
            `([stx ,stx] [pstx ,pstx] [nodeID ,(gensym 'binexp)]))]
     [(and (? syntax? e)
           (? (lambda (o)
                (or (integer? (syntax-object->datum o))
                    (and (identifier? o)
                         (member (syntax-e o) '(true false)))))
              e)
           e)
      (<literal> e pstx)]
     [(? identifier? e) (<variable> e pstx)]
     [err (<err> err pstx)]
     ))
  
  (define-production (literal expr pstx)
    (table parse-production-table)
    (relies-on err)
    (matches
     [(and (? syntax? e) 
           (? (lambda (o)
                (integer? (syntax-object->datum o))) e) e)
      (im! (make-literal (empty-meta) (syntax-object->datum e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'int)]))]
     [(and (? syntax? e) 
           (? (lambda (o)
                (and (identifier? o)
                     (member (syntax-e o) '(true false)))) e)
           e)
      (im! (make-literal (empty-meta) (syntax-e e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'bool)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (formal expr pstx)
    (table parse-production-table)
    (relies-on name type err)
    (matches
     [`(,(? syntax? stx)
         ,n ,t)
       (im! (make-formal (empty-meta)
                         (<name> n stx) (<type> t stx))
            `([stx ,stx] [pstx ,pstx][nodeID ,(gensym 'formal)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (name expr pstx)
    (table parse-production-table)
    (relies-on err)
    (matches
     [(? identifier? e) 
      (im! (make-name (empty-meta) (syntax-object->datum e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'name)]))]
     [err (<err> err pstx)]
     ))
  
  (define-production (type expr pstx)
    (table parse-production-table)
    (relies-on err)
    (matches
     [(? identifier? e)
      (im! (make-type (empty-meta) (syntax-object->datum e))
           `([stx ,e] [pstx ,pstx] [nodeID ,(gensym 'type)]))]
     [err 
      (debug parse (printf "Trying to handle: ~a~n" expr))
      (<err> err pstx)]
     ))
 
  ;;
  ;; THE PASS
  ;;
  ;; Notice; we don't have an error handler, because we override
  ;; it with a production-specific error handler in every instance.
  ;; No catch-all for us!
  (define-pass parse
    ([mod process]
     [process action construction instance specification (err process-err)]
     [definition name formal process (err definition-err)]
     [declaration name type process (err declaration-err)] 
     [construction seq conditional loop par (err construction-err)]
     [specification declaration definition (err specification-err)]
     [instance name (err instance-err)]
     [seq process (err seq-par-err)]
     [par process (err seq-par-err)]
     [conditional choice (err conditional-err)]
     [choice expression process (err choice-err)]
     [loop expression process (err loop-err)]
     [action assignment input output (err action-err)]
     [assignment variable expression (err assignment-err)]
     [input chan variable (err input-err)]
     [output chan expression (err output-err)]
     [expression literal variable (err expression-err)]
     [literal (err literal-err)]
     [chan (err chan-err)]
     [variable (err variable-err)]
     [name (err name-err)]
     [type (err type-err)]
     [formal name type (err formal-err)]
     ))
  
  )
