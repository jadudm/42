(module uniquely-name-variables mzscheme
  (provide uniquely-name-variables)
  
  (require (lib "macros/grammar.scm" "42")
           (lib "structures.scm" "42")
           (lib "exceptions.scm" "42")
           (lib "productions.scm" "42")
           (lib "debug.scm" "42")
           (lib "identities/identity0.scm" "42/passes")
           )
  
  (define unv-production-table 
    (copy-production-table empty-production-table))
  
  
    ;;
  ;; ENVIRONMENTS
  ;; 
  
  ;; The empty environment.
  (define (empty-env) '())
  
  ;; Store env pairs in a structure; cleaner
  (define-struct env-pair (old new) (make-inspector))
  
  ;; append-number :: symbol number -> symbol
  ;; glues the symbol and the number together to 
  ;; form one symbol
  (define (append-number sym number)
    (string->symbol (format "~a.~a" sym  number)))

  ;; extend-env :: list symbol -> list
  ;; purpose
  ;; Extends the variable renaming environment
  ;; with a new symbol.
  (define extend-env
    (let ([counter 0])
      (lambda (env sym)
        (set! counter (add1 counter))
        (debug env 
          (printf "Extending the environment: '~a' => '~a'~n"
                  sym (append-number sym counter)))
        (let ([extended
               (cons (make-env-pair 
                      sym 
                      (append-number sym counter)) env)])
          (debug env (printf "Extended to: ~s~n" extended))
          extended
          ))))
  
  ;; search-env :: list symbol -> list | #f
  ;; searches the list for an occurence of the symbol
  ;; and returns the first instance found.
  (define (search-env env sym)
    (define (search-help env sym)
      (cond
        [(null? env) #f]
        [(begin
           (debug env 
             (printf "Comparing ~a <-> ~a~n"
                     sym (env-pair-old (car env))))
           (equal? sym (env-pair-old (car env))))
         (car env)]
        [else
         (search-help (cdr env) sym)]))
    (search-help env sym))
  
  ;; Both get-old-name and get-new-name extract symbols from
  ;; pairs of values in the environment. 
  
  ;; get-new-name :: list symbol symbol -> symbol
  ;; Takes an environment list and a name to lookup
  ;; and returns the unique name.
  (define (get-new-name env meta sym)
    (debug env (printf "Getting new name...~n"))
    (debug env (printf "Env is: ~a~n" env))
    (debug env (printf "Sym is: ~s~n" sym))
    (let ([res (search-env env sym)]
          [stx (get-meta meta 'stx)]
          [pstx (get-meta meta 'pstx)])
      (debug env (printf "Binding ~a -> ~a~n" sym res))
      (if res
          ;;FIXME:  Break this out into another function -
          ;; chan, variable and instance use the same form. 
          (env-pair-new res)
          ;;(raise-syntax-error (syntax-e stx) "variable undefined" pstx stx )
          (exn42 name
                 undefined-name
                 (format
                  (tab-append
                   "You have used '~a', but have neither:~n"
                   " 1. Declared it in a 'decl', or~n"
                   " 2. Included it in the formals of a 'proc'.~n"
                   "Please try again.")
                  (syntax-e stx))
                 stx pstx)

          )))  
  

  
  ;; WARNING / FIXME
  ;; Currently, all the productions in this pass have the same signatures
  ;; as every other pass. However, every production in this pass
  ;; expects two arguments. Therefore, the signature should probably
  ;; change, so they can't be mixed in with productions that only
  ;; have one argument. It would be safer.
  
  (define-production (err expr)
    (table unv-production-table)
    (relies-on)
    (matches
     [any 
      (exn42 catch-all unv
             (format
              (tab-append 
               "No idea what happened, but it probably was bad."))
             (e&gm expr 'stx)
             (e&gm expr 'pstx))]))
  
  ;; WARNING
  ;; We are assuming that we'll never pass an environment
  ;; into a module. This... may be OK, until we start passing
  ;; names from one module to another. But that can be a 
  ;; problem for another day.
  (define-production (mod expr)
    (table unv-production-table)
    (relies-on process err)
    (matches
     [(struct mod (meta process+))
      (debug unv (printf "mod~n"))
      (make-mod meta (map (lambda (p)
                            (<process> p (empty-env))) process+))]
     ;;                                  ^^^^^^^^^^^
     [err (<err> err )]
     ))
  
  (define-production (process expr env)
    (table unv-production-table)
    (relies-on action construction instance specification err)
    (matches
     [(? stop? s) s]
     [(? skip? s) s]
     [(? action? s) (<action> s env)]
     [(? specification? s) (<specification> s env)]
     [(? construction? s) (<construction> s env)]
     [(? instance? s) (<instance> s env)]
     [err (<err> err )]
     ))
  
  (define-production (specification expr env)
    (table unv-production-table)
    (relies-on declaration definition err)
    (matches
     [(? declaration? s) (<declaration> s env)]
     [(? definition? s) (<definition> s env)]
     [err (<err> err )]
     ))
  
  (define-production (instance expr env)
    (table unv-production-table)
    (relies-on name err)
    (matches
     [(struct instance (meta name actual*))
      (debug unv (printf "instance~n"))
      (make-instance meta 
                     (<name> name env)
                     (map (lambda (a)
                            (make-name
                             (NM a)
                             (get-new-name env meta (name-sym a))))
                          actual*))]
     ))
  
  (define-production (definition expr env)
    (table unv-production-table)
    (relies-on name formal process err)
    (matches
     [(struct definition (meta name formal* process))
      (debug unv (printf "definition~n"))
      (let* ([new-env env]
             [new-formals
              ;; Extend the environment with the formals
              (for-each
               (lambda (f)
                 (let ([name-struct (formal-name f)]
                       [type-struct (formal-type f)])
                   (let ([name-sym (name-sym name-struct)])
                     (set! new-env (extend-env new-env name-sym)))))
               formal*)])
        
        ;; Create a new structure, passing on the new-env
        (make-definition meta 
                         (<name> name env) 
                         (map (lambda (f)
                                (<formal> f new-env)) formal*) 
                         (<process> process new-env)))]
     [err (<err> err )]
     ))
  
  (define-production (formal expr env)
    (table unv-production-table)
    (relies-on name type err)
    (matches
     [(struct formal (meta name type))
      (debug unv (printf "formal~n"))
      (make-formal meta 
                   (make-name
                    (NM name)
                    (get-new-name env meta (name-sym name)))
                   type)]
     [err (<err> err )]
     ))
  
  (define-production (action expr env)
    (table unv-production-table)
    (relies-on assignment input output err)
    (matches 
     [(? assignment? s) (<assignment> s env)]
     [(? input? s) (<input> s env)]
     [(? output? s) (<output> s env)]
     [err (<err> err )]
     ))
  
  (define-production (assignment expr env)
    (table unv-production-table)
    (relies-on variable expression err)
    (matches
     [(struct assignment (meta var exp)) 
      (debug unv (printf "assignment~n"))
      (make-assignment meta
                       (<variable> var env)
                       (<expression> exp env))]
     [err (<err> err )]
     ))
  
  (define-production (input expr env)
    (table unv-production-table)
    (relies-on chan variable err)
    (matches
     [(struct input (meta chan var))
      (debug unv (printf "input~n"))
      (make-input meta (<chan> chan env) (<variable> var env))]
     ))
  
  (define-production (output expr env)
    (table unv-production-table)
    (relies-on chan expression err)
    (matches
     [(struct output (meta chan exp))
      (debug unv (printf "output~n"))
      (make-output meta (<chan> chan env) (<expression> exp env))]
     ))
  
  
  (define-production (chan expr env)
    (table unv-production-table)
    (relies-on err)
    (matches
     [(struct chan (meta sym))
      (debug unv (printf "chan~n"))
      ;; Error handling embedded in 'get-new-name'
      (make-chan meta (get-new-name env meta sym))]
     ))
  
  (define-production (variable expr env)
    (table unv-production-table)
    (relies-on err)
    (matches
     [(struct variable (meta sym)) 
      (debug unv (printf "variable~n"))
      ;; Error handling embedded in 'get-new-name'
      (make-variable meta (get-new-name env meta sym))]
     [err (<err> err )]
     ))
  
  (define-production (expression expr env)
    (table unv-production-table)
    (relies-on literal variable err)
    (matches
     [(struct monadic-expression (meta rator rand)) 
      (debug unv (printf "monadic-expression~n"))
      (make-monadic-expression meta rator (<expression> rand env))]
     [(struct dyadic-expression (meta rator rand1 rand2)) 
      (debug unv (printf "dyadic-expression~n"))
      (make-dyadic-expression meta rator (<expression> rand1 env)(<expression> rand2 env))]
     [(? literal? e) (<literal> e env)]
     [(? variable? e) (<variable> e env)]
     [err (<err> err )]
     ))
  
  
  (define-production (name expr env)
    (table unv-production-table)
    (relies-on err)
    (matches
     [(? name? e) 
      (debug unv (printf "name~n"))
      e]
     [err (<err> err )]))
  
  (define-production (type expr env)
    (table unv-production-table)
    (relies-on err)
    (matches
     [(? type? e) 
      (debug unv (printf "type~n"))
      e]
     [err (<err> err )]
     ))
  
  (define-production (literal expr env)
    (table unv-production-table)
    (relies-on err)
    (matches
     [(? literal? e) 
      (debug unv (printf "literal~n"))
      e]
     [err (<err> err )]
     ))
  
  (define-production (declaration expr env)
    (table unv-production-table)
    (relies-on name type process err)
    (matches 
     [(struct declaration (meta name-type+ process)) 
      (debug unv (printf "declaration~n"))
      (let* ([new-env env]
             [new-name-type-list 
              (map (lambda (pair)
                     ;; name is a variable-struct
                     ;; type is a type-struct
                     ;; We want to rename the variable
                     ;; and return a new pair, as well
                     ;; as extend the environment
                     (let ([name (car pair)]
                           [type (cadr pair)])
                       ;; Extend the environment with a new symbol.
                       ;; This will take, say, 'x, and create a new
                       ;; pair in the environment, (list 'x 'x.4)
                       ;; We can overwrite the environment here, because
                       ;; it is local to declaration.
                       (set! new-env (extend-env new-env (name-sym name)))
                       (let ([new-name 
                              (make-name
                               (NM name)
                               (get-new-name new-env meta (name-sym name)))])
                         (list new-name type)
                         )))  ;; end of lambda in map
                   name-type+)])
        (make-declaration meta
                          new-name-type-list 
                          (<process> process new-env)))]
     [err (<err> err )]
     ))
  
  (define-production (construction expr env)
    (table unv-production-table)
    (relies-on seq conditional loop par err)
    (matches
     [(? seq? s) (<seq> s env)]
     [(? par? s) (<par> s env)]
     [(? conditional? s) (<conditional> s env)]
     [(? while?  s) (<loop> s env)]
     [err (<err> err )]
     ))
  
  (define-production (seq expr env)
    (table unv-production-table)
    (relies-on process err)
    (matches
     [(struct seq (meta process+))
      (debug unv (printf "seq~n"))
      (make-seq meta (map (lambda (p)
                            (<process> p env))
                          process+))]
     [err (<err> err )]
     ))
  
  (define-production (par expr env)
    (table unv-production-table)
    (relies-on process err)
    (matches
     [(struct par (meta process+))
      (debug unv (printf "par~n"))
      (make-par meta (map (lambda (p)
                            (<process> p env))
                          process+))]
     [err (<err> err )]
     ))
  
  (define-production (conditional expr env)
    (table unv-production-table)
    (relies-on choice err)
    (matches
     [(struct conditional (meta choice+))
      (debug unv (printf "conditional~n"))
      (make-conditional meta (map (lambda (c)
                                    (<choice> c env))
                                  choice+))]
     [err (<err> err )]
     ))
  
  (define-production (choice expr env)
    (table unv-production-table)
    (relies-on expression process err)
    (matches
     [(struct choice (meta exp process))
      (debug unv (printf "choice~n"))
      (make-choice meta (<expression> exp env) (<process> process env))]
     [err (<err> err )]
     ))
  
  (define-production (loop expr env)
    (table unv-production-table)
    (relies-on expression process err)
    (matches
     [(struct while (meta exp process))
      (debug unv (printf "loop~n"))
      (make-while meta (<expression> exp env) (<process> process env))]
     [err (<err> err )]
     ))
  #|
  #;(create-production-table unv-production-table
      mod process definition declaration construction
      specification instance seq par conditional choice
      loop action assignment input output expression literal variable
      name type chan formal)
  |#
  
  (define uniquely-name-variables 
    (make-identity0-grammar unv-production-table))
  
  )