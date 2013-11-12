(module grammar-helpers mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  ;; Linkages look like
  ;; #`(#,p : #,p^ (#,p@ #,@b*)))
  ;; However, there's a few things to note:
  ;; A b* may contain either tags, or a list. 
  ;; A list would be a pair, like
  ;; (tag new-unit)
  ;; meaning "Use the unit from this pair, but
  ;; use the tag for everything else. Also,
  ;; use the tag in the binding list instead of the new-unit.
  
  
  ;; Simple bindings implies a define-pass rule like:
  ;; [process construction err]
  ;; where there are no list subforms.
  (define (get-simple-bindings stx:p* stx:b**)
    (filter list? 
            (map (lambda (p b*)
                   (if (andmap identifier? b*)
                       (list p b*))) stx:p* stx:b**)))
  
  ;; A complex binding implies something like
  ;; [process construction (err new-err)]
  ;; where the err-form is being overridden.
  (define (get-complex-bindings stx:p* stx:b**)
    (filter list? 
            (map (lambda (p b*)
                   (if (andmap identifier? b*)
                       (void)
                       (list p b*))) stx:p* stx:b**)))
  
  (define (make-simple-linkages simple-bindings)
    (map (lambda (p-b*)
           (let ([p (car p-b*)]
                 [b* (cadr p-b*)])
             (let ([p^ (add-postfix p '^)]
                   [p@ (add-postfix p '@)])
               #`(#,p : #,p^ (#,p@ #,@b*)))))
         simple-bindings))
  
  (define (make-complex-linkages complex-bindings)
    (map (lambda (p-b*)
           (let ([p (car p-b*)]
                 [b* (cadr p-b*)])
             (let ([p^ (add-postfix p '^)]
                   [p@ (add-postfix p '@)]
                   ;; If we have a pair like
                   ;; (b new-b)
                   ;; we want to use the tag for the new
                   ;; binding here. It will get magically 
                   ;; introduced into the compound unit by the macro.
                   [trimmed-b*
                    (map (lambda (b)
                           (if (not (identifier? b)) 
                               (cadr (syntax->list b))
                               b)) b*)])
               #`(#,p : #,p^ (#,p@ #,@trimmed-b*)))))
         complex-bindings))
  
  (define (make-new-linkage b simple complex)
    (let* ([tag (car (syntax->list b))]
           [tag^ (add-postfix tag '^)]
           [new-unit (cadr (syntax->list b))]
           [new-unit@ (add-postfix new-unit '@)])
      ;; Now, I need to find the bindings for this tag.
      (let ([bindings '()])
        ;; If it is a simple binding, I'm safe. Just
        ;; slap the b* in there, and be done.
        (for-each (lambda (p-b*)
                    (let ([p (car p-b*)]
                          [b* (cadr p-b*)])
                      #;(printf "Simple: ~a <-> ~a~n" 
                                (syntax-object->datum p)
                                (syntax-object->datum tag))
                      (if (equal? (syntax-object->datum p)
                                  (syntax-object->datum tag))
                          (set! bindings b*))))
                  simple)
        ;; If it is a complex binding I'm dealing with,
        ;; I need to simplify the b* before using it; 
        ;; just like the trimmed-b* in the above function.
        (for-each (lambda (p-b*)
                    (let ([p (car p-b*)]
                          [b* (cadr p-b*)])
                      (if (equal? (syntax-object->datum p)
                                  (syntax-object->datum tag))
                          (set! bindings
                                (map (lambda (b)
                                       (if (not (identifier? b)) 
                                           (cadr (syntax->list b))
                                           b)) b*)
                                ))))
                  complex)
        ;; Leftover from debugging
        ;; #`(quote (#,new-unit -> #,bindings))
        #`(#,new-unit : #,tag^ (#,new-unit@ #,@bindings))
        )))
  
  (define (make-new-linkages simple complex)
    (let ([new-linkages
           (map (lambda (p-b*)
                  (let ([p (car p-b*)]
                        [b* (cadr p-b*)])
                    (filter (lambda (o)
                              (not (void? o)))
                            (map (lambda (b)
                                   (if (not (identifier? b)) 
                                       (make-new-linkage b simple complex)
                                       (void)
                                       )) b*))))
                complex)])
      ;; new-linkages is a list of lists, but only one element in each
      ;; of those lists; so, I'll map car over them.
      ;;
      ;; However, I also need to make sure that there aren't any duplicate
      ;; linkages. This can be done by checking that the car of each
      ;; of these sublists is unique. If not, drop it.
      (define (helper lolinkages)
        #;(printf "list? ~a ~a~n" (list? lolinkages) lolinkages)
        (cond
          [(null? lolinkages) '()]
          ;; If this tag is repeated...
          [(let ([first-tag (syntax-e (car (syntax-e (car lolinkages))))]
                 [list-of-firsts
                  (map syntax-e
                       (map car 
                            ;; Make sure you only search the rest 
                            ;; of the list!
                            (cdr (map syntax->list lolinkages))))])
             #;(printf "first-tag: ~a~n" first-tag)
             #;(printf "list-of-firsts: ~a~n" list-of-firsts)
             (member first-tag list-of-firsts))
           ;; Drop it.
           (helper (cdr lolinkages))]
          ;; Otherwise, keep it.
          [else (cons (car lolinkages)
                      (helper (cdr lolinkages)))]))
        (helper (map car new-linkages))))
  
  (define (make-linkages stx:production* stx:binding**)
    (let ([stx:p* (syntax->list stx:production*)]
          [stx:b** (map syntax->list (syntax->list stx:binding**))])
      ;; First, I'll separate out those bindings that will require additional work.
      (let ([simple-bindings (get-simple-bindings stx:p* stx:b**)]
            [complex-bindings (get-complex-bindings stx:p* stx:b**)])
        ;; Now, I can start creating linkages. The simple ones 
        ;; are of the form 
        ;; #`(#,p : #,p^ (#,p@ #,@b*)))
        ;; Complex linkages require me to pull apart some lists;
        ;; effectively, the same thing, but requires a bit of checking.
        ;; Hence, it is done in a separate step. 'New linkages' are
        ;; those that get introduced due to the complex bindings; they're
        ;; 'magic' in that the programmer doesn't declare them, but we know
        ;; 1. Because they are declared as (tag new-unit), we know the tag
        ;;    of the unit and signature they are equivalent to.
        ;; 2. We can look that up, and build a new linkage like:
        ;;    #`(#,new-unit : #,old-sig (#,new-unit@ #,@old-tag-bindings)
        ;;    so that it will work just like the original unit. 
        (let ([simple-linkages (make-simple-linkages simple-bindings)]
              [complex-linkages (make-complex-linkages complex-bindings)]
              [new-linkages (make-new-linkages simple-bindings complex-bindings)])
          #`(#,@simple-linkages
                #,@complex-linkages
                #,@new-linkages)
          ))))
  
  
  
  (define (add-postfix stx sym) 
    (datum->syntax-object
     stx
     (string->symbol
      (format "~a~a" (syntax-e stx) sym))))
  
  (define (add-prefix stx sym)
    (datum->syntax-object
     stx
     (string->symbol
      (format "~a~a" sym (syntax-e stx)))))
  
  (define (bracket stx)
    (add-postfix (add-prefix stx "<") ">"))
  
  
  )
