(module soccam mzscheme
  (require-for-syntax (lib "passes/parsing/expand.scm" "42")
                      (lib "driver.scm" "42")
                      (lib "debug.scm" "42")
                      (lib "structures.scm" "42")
                      (lib "exceptions.scm" "42"))
  (provide (rename app #%app)
           (rename datum #%datum)
           (rename top #%top)
	   #%module-begin)
  
  (begin-for-syntax
    (define (handle-exn42-drscheme e)
      (raise-syntax-error
       (exn:exn42-id e)
       (format "~n~a~n" (exn:exn42-msg e))
       (exn:exn42-stx e)
       (exn:exn42-pstx e))))
  
  (define-syntax (app stx)
    (syntax-case stx ()
      [(_ args ...)
       (let ([just-module
              (syntax (args ...))])
         (with-handlers ([exn:exn42?
                          handle-exn42-drscheme])
           (run just-module))
         #`(quote Done))
         ]))

  
  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ args ...)
       (printf "Module syntax: ~a~n"
               (syntax-object->datum stx))
       (quote-syntax module)]
      [(_ . arg)
       (syntax '())]))
  
  (define-syntax (top stx)
    (syntax-case stx ()
      [(_ args ...)
       (syntax '())]
      [(_ . foo)
       (syntax '())]
      ))
  
  (define-syntax (datum stx)
    (syntax-case stx ()
      [(_ . arg)
       (syntax '())]))
  
  
  
  )

#|

  (define (handle-exn42-drscheme e)
    (raise-syntax-error
     (exn:exn42-id e)
     (format "~n~a~n" (exn:exn42-msg e))
     (exn:exn42-stx e)
     (exn:exn42-pstx e)))
  
  (define first-press? #t)
  (define program '())
  (define (false? o) (not o))
  (define (compile-gathered-syntax)
    ;;(printf "Compiling program.~n")
    ;;(add-debug-flag! 'driver)
    (print-struct #t)
    (with-handlers ([exn:exn42? handle-exn42-drscheme])
		   (run program)))

(define/public (get-reader)
	    (lambda (src port)
	      (let ([v (read-syntax src port)])
		(cond
		 [(and (eof-object? v)
		       first-press?)
		  (set! first-press? #f)
		  (syntax (quote Ready))]
		 [(eof-object? v)
		  (compile-gathered-syntax)
		  v]
		 [else
		  (set! program v)
		  (syntax
		   (string->symbol "Compiling..."))]))))
|#