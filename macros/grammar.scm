(module grammar mzscheme

  (require-for-syntax "grammar-helpers.scm")
  (require (lib "unitsig.ss") 
           (lib "plt-match.ss"))

  (provide (all-defined))
  
  ;; This actually drops the (declare-siganture foo^ (<foo>)) 
  ;; lines that declare the existance of the signature, and 
  ;; the name of the function exported from that signature.
  (define-syntax declare-productions
    (lambda (stx)
      (syntax-case stx ()
        [(_ (ls ...))
         (let ([sigs  (map (lambda (stx) (add-postfix stx '^))
                           (syntax->list (syntax (ls ...))))]
               [brkts (map (lambda (stx) (bracket stx)) 
                           (syntax->list (syntax (ls ...))))])
           #`(begin
              (begin
               #,@(map (lambda (sig brkt)
                         #`(define-signature #,sig (#,brkt)))
                       sigs brkts)))
           )]
        )))
  
  ;; The define pass macro is really just a compound unit. 
  ;; It links a bunch of productions (signed units) into
  ;; a single compound unit, which we then instantiate as a 
  ;; compiler pass. The topmost form becomes the starting
  ;; point for the pass.
  ;;
  ;; Typically, it would be used as follows:
  ;;
  #;(define-pass foo
      ([let expr variable]
       [expr literal variable]
       [literal]
       [variable]))
  ;;
  ;; This would imply a pass that roughly corresponded to the grammar
  ;;
  ;; <let>      := ... <expr> ... <variable> ...
  ;; <expr>     := ... <literal> ... <variable> ...
  ;; <literal>  := ...
  ;; <variable> := ...
  ;;
  ;; I haven't filled the grammar in; the only thing we're really interested
  ;; in with the 'define-pass' macro is how the grammar is recurred through.
  ;; So, we're interested in the fact that <let> *relies-on* <expr> and <variable>, 
  ;; and <expr> *relies-on* <literal> and <variable>. The implementation of these
  ;; forms is defined by four 'define-production' forms, one for each
  ;; <bracketed form> in the grammar.
  ;;
  ;; In this case, <let> would become the export of 'define-pass', and be
  ;; the point that this particular compiler pass begins. Our pseudoprogram 
  ;; is starting to look like:
  ;;
  #;(define-production (let e) ...)
  #;(define-production (expr e) ...)
  #;(define-production (literal e) ...)
  #;(define-production (variable e) ...)
  #;(define-pass foo
      ([let expr variable]
       [expr literal variable]
       [literal]
       [variable]))
  ;; 
  ;; Now, I can override productions on a per-production basis.
  ;; For example, I might want to use a different production for
  ;; handling <variable>s in the <let> form. 
  ;;
  ;; My pass would look like this:
  ;;
  #;(define-pass foo
      ([let expr (variable let-var)]
       [expr literal variable]
       [literal]
       [variable]))
  ;;
  ;; And my pseudoprogram would now look like:
  ;;
  #;(define-production (let e) ...)
  #;(define-production (expr e) ...)
  #;(define-production (literal e) ...)
  #;(define-production (variable e) ...)
  #;(define-production (let-var e) (export variable) ...)
  #;(define-pass foo
      ([let expr (variable let-var)]
       [expr literal variable]
       [literal]
       [variable]))
  ;;
  ;; The macro automatically expands the compound unit to take
  ;; into account this new production, <let-var>, that is
  ;; expected to have the same signature as <variable>. We
  ;; have to annotate the production with an 'export' form.  
  (define-syntax (define-pass stx)
    (syntax-case stx ()
      [(_ name ([production* binding** ...] ...))
       (let* ([top (car (syntax->list (syntax (production* ...))))]
              [top-bracket (bracket top)]             
              [at-name (add-postfix (syntax name) '@)]
              [linkages (make-linkages
                         (syntax (production* ...)) 
                         (syntax ((binding** ...) ...)))])
             
         ;; #`(quote #,linkages)
         #`(begin
             (begin
               (define #,at-name
                 (compound-unit/sig
                   (import)
                   (link
                    #,@linkages
                    )
                   (export (var (#,top #,top-bracket)))
                   ))
               (define name
                 (let ()
                   (define-values/invoke-unit/sig (#,top-bracket)
                                                  #,at-name)
                   #,top-bracket))))
         )]))
  
  
  ;; Defines a hash table with bindings matching
  ;; the symbols used in 'names' and units derived from
  ;; those names. For example:
  ;;
  ;; (create-production-table foo a b c)
  ;;
  ;; yields:
  ;; (define foo-production-table (make-hash-table))
  ;; (hash-table-put! foo-production-table 'a a@)
  ;; (hash-table-put! foo-production-table 'b b@)
  ;; (hash-table-put! foo-production-table 'c c@)
  ;;
  ;; Which is then passed on to the grammar-maker produced
  ;; by 'make-pass'.
  
  (define copy-production-table hash-table-copy)
  (define (make-table-immutable hash)
    (make-immutable-hash-table
     (hash-table-map
      hash
      (lambda (k v) (cons k v)))))
    
  (define empty-production-table (make-immutable-hash-table '()))
  
  (define-syntax (create-production-table stx)
    (syntax-case stx ()
      [(_ table-name names ...)
       (let ([keys (map (lambda (stx)
                          #`(quote #,(syntax-e stx)))
                        (syntax->list (syntax (names ...))))]
             [values (map (lambda (stx)
                            (add-postfix stx '@))
                          (syntax->list (syntax (names ...))))]
             )
         
         #`(begin
             (define table-name 
               (make-immutable-hash-table
                (list 
                 #,@(map (lambda (key val)
                           #`(cons #,key #,val))
                         keys 
                         values))))
             ))]))
  
  (define-syntax (insert-productions stx)
    (syntax-case stx ()
      [(_ hash names ...)
       (let ([keys (map (lambda (stx)
                          #`(quote #,(syntax-e stx)))
                        (syntax->list (syntax (names ...))))]
             [values (map (lambda (stx)
                            (add-postfix stx '@))
                          (syntax->list (syntax (names ...))))])
         
         #`(begin
             #,@(map (lambda (key val)
                       #`(begin
                           ;;(printf "Inserting new: ~a ~a~n" #,key #,val)
                           (hash-table-put! #,(syntax hash) #,key #,val)))
                     keys 
                     values)))]))
       
  (define-syntax (make-pass stx)
    (syntax-case stx ()
      [(_ ([production* binding** ...] ...))
       (let* ([top (car (syntax->list (syntax (production* ...))))]
              [top-bracket (bracket top)]             
              [at-name (add-postfix (syntax name) '@)]
              [linkages (make-linkages
                         (syntax (production* ...)) 
                         (syntax ((binding** ...) ...)))])
             
             
         ;; #`(quote #,linkages)
         #`(lambda (h)
             ;; Bind all the signatures. The hash table should
             ;; have been created with 'create-production-table'
             #,@(map (lambda (p)
                       #`(define #,(add-postfix p '@)
                           (begin
                             ;;(printf "Looking up ~a~n" (quote #,p))
                             (hash-table-get 
                              h (quote #,p)
                              (lambda ()
                                (printf 
                                 (string-append
                                  "*****************************************************~n"
                                  "**             INTERNAL COMPILER ERROR             **~n"
                                  "*****************************************************~n"
                                  "Binding for '~a' not provided "
                                  "when creating a production table.~n")
                                 (quote #,p))
                                (exit)))
                             )))
                     (syntax->list (syntax (production* ...)))) 
             (let ()
               (define #,at-name
                 (compound-unit/sig
                   (import)
                   (link
                    #,@linkages
                    )
                   (export (var (#,top #,top-bracket)))
                   ))
               (define-values/invoke-unit/sig (#,top-bracket)
                                              #,at-name)
               #,top-bracket))
         )]))
  
  
  
  (define-syntax (define-override-production stx)
    (syntax-case stx (relies-on matches export)
      [(_ (lhs body-args ...) (export exp) (relies-on bindings ...) (matches bodies ...))
       (let ([lhs-signature (add-postfix (syntax lhs)  '@)]
             [lhs-bracket   (bracket (syntax exp))]
             [lhs^          (add-postfix (syntax exp) '^)]
             [imports^      (map (lambda (stx) (add-postfix stx '^)) 
                                 (syntax->list (syntax (bindings ...))))]
             [first-arg     (let ([ls (syntax->list (syntax (body-args ...)))])
                              (if (list? ls)
                                  (car ls)
                                  (error 'production-rule
                                         "Must have an argument to production.~n")))] )
         
         #`(begin
            (define #,lhs-signature
              (unit/sig #,lhs^
                (import #,@imports^)
                (define (#,lhs-bracket #,@(syntax->list (syntax (body-args ...))))
                  (match #,first-arg 
                    #,@(syntax->list (syntax (bodies ...))))))))
       )]))
  
  (define-syntax (define-production stx)    
    (syntax-case stx (relies-on matches export table)
      ;; 20060612 MCJ
      ;; Now that I have a better understanding of how the expansion-time
      ;; transformation takes place, and a better understanding of syntax-case
      ;; and syntaxes in general, I should be able to rewrite this so
      ;; there are a number of optional pairs (eg. export, relies-on, etc.)
      ;; before the matches form. 
      
      ;; What was 'add-production'
      [(_ (lhs body-args ...) (table table-name) (relies-on bindings ...) (matches bodies ...))
       (let ([lhs-signature (add-postfix (syntax lhs) '@)]
             [lhs-bracket   (bracket (syntax lhs))]
             [lhs^          (add-postfix (syntax lhs) '^)]
             [imports^      (map (lambda (stx) (add-postfix stx '^)) 
                                 (syntax->list (syntax (bindings ...))))]
             [first-arg     (let ([ls (syntax->list (syntax (body-args ...)))])
                              (if (list? ls)
                                  (car ls)
                                  (error 'production-rule
                                         "Must have an argument to production.~n")))] 
             [key #`(quote lhs)]
             )
         
         #`(begin
            (define #,lhs-signature
              (unit/sig #,lhs^
                (import #,@imports^)
                (define (#,lhs-bracket #,@(syntax->list (syntax (body-args ...))))
                  (match #,first-arg 
                    #,@(syntax->list (syntax (bodies ...)))))))
            ;;(printf "Adding ~a -> ~a~n" #,key #,lhs-signature)
            (hash-table-put! table-name #,key #,lhs-signature)
            )
         )]
      ))
    
  )