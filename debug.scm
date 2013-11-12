(module debug mzscheme
  (require (lib "pregexp.ss"))
  (provide (all-defined))
  
  (define *debug-flags* '())
  (define *no-flag* '())
  (define *disable-flags* '())
  
  (define-syntax (debug stx)
    (syntax-case stx ()
      [(_ tag exp ...)
       #`(if (member (quote tag) *debug-flags*)
             (if (member (quote tag) *no-flag*)
                 (begin exp ...)
                 (begin
                   (printf "~a: " (quote tag))
                   exp ...)))
       ]))
  
  (define-syntax (disable stx)
    (syntax-case stx ()
      [(_ tag exp ...)
       #`(if (not (member (quote tag) *disable-flags*))
             (begin
               exp ...))]))
  
  (define (add-debug-flag! str)
    (let ([sym (void)])
      (if (symbol? str)
          (set! str (symbol->string str)))

      (let* ([no-flag? (pregexp-match "^silent:" str)]
             [w/o-flag (pregexp-replace "^silent:" str "")])
        (set! sym (string->symbol w/o-flag))

        (if no-flag? 
            (set! *no-flag* (cons sym *no-flag*)))
        
        (set! *debug-flags* (cons sym *debug-flags*)))))
  
  (define (add-disable-flag! sym)
    (if (string? sym)
        (set! sym (string->symbol sym)))
    (set! *disable-flags* (cons sym *disable-flags*)))
  
  (define (clear-debug-flags! sym)
    (set! *debug-flags* '()))
  
  (define (debug-flag? sym)
    (member sym *debug-flags*))
  
 )