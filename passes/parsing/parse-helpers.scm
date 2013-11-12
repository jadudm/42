(module parse-helpers mzscheme
  (require (lib "debug.scm" "42"))
  (provide (all-defined))
  
  ;; ssym? :: symbol -> stx-obj -> boolean
  ;; This curried procedure takes a symbol, which is then compared against
  ;; a syntax object that is expanded once by 'syntax-e'. 
  (define ssym?
    (lambda (sym)
      (lambda (obj)
        (if (identifier? obj)
            (begin
              (debug parse
                (printf "ssym? : checking ~a against (stx ~a)~n" 
                        sym (syntax-object->datum obj)))
              (equal? (syntax-e obj) sym))
            #f))))
    
  ;; Now expects an expression that starts with
  ;; a syntax object, and then the 'start' of the list.
  ;; That start, though, is a syntax object containing
  ;; a single symbol (hopefully). 
  (define first-sym?
    (lambda (sym)
      (lambda (exp)
        (and (list? exp)
             (syntax? (car exp))
             (equal? sym (syntax-object->datum (second exp)))))))
  
                 
  ;; Quick shorthand for the next section...
  (define second cadr)
  
  ;; We're using 'second' instead of 'first', 
  ;; because these lists now have a syntax object at the head
  ;; instead of a symbol of some sort. We want to skip that
  ;; syntax object, and instead compare against the 
  ;; symbol in the second position.
  (define (sexp:action? e)
    (and (list? e)
         (identifier? (second e))
         (member (syntax-e (second e)) '(:= ! ?))))
  
  ;; Construction forms; may grow, may change, may not.
  ;; I'd like to get rid of the occam-y 'if', and make
  ;; it behave like a normal, two-armed 'if'.
  ;; I would call the cascading form 'cond'.
  (define (sexp:construction? e)
    (and (list? e)
         (identifier? (second e))
         ;; WARNING
         ;; Hacked if/cond
         (member (syntax-e (second e)) '(seq par while if cond))))
  
  ;; Not many of these at the moment. I'd kinda
  ;; like to introduce a 'let' form, where
  ;; the RHS of the variable declarations are expressions.
  ;; This implies that I can introduce (void), though, so
  ;; that it is possible to declare a variable in a 'let',
  ;; but assign no value to it. But... would 'void'
  ;; be legal anywhere else? Sounds like a big language 
  ;; change to me...
  (define (sexp:specification? e)
    (and (list? e)
         (identifier? (second e))
         (member (syntax-e (second e)) '(decl proc))))
  
  ;; Checks to see that everything in the 
  ;; list is an identifier. Really, this doesn't
  ;; tell us a lot, but we assume it is something of the form
  ;;
  ;; (some-proc arg1 arg2 arg3)
  ;;
  ;; Currently, that is OK, because we're not allowing 
  ;; forms along the lines of 
  ;;
  ;; (some-proc arg1 (+ 3 5) arg3)
  ;;
  ;; which may, or may not, ever be legal.
  (define (sexp:inst? e)
    (and (list? e)
         (identifier? (second e))
         (andmap (lambda (s) (identifier? s))
                 (cddr e))))
  
  ;; All the symbols we expect to identify a 
  ;; unary operation.
  (define (sexp:monadic? sym)
    (and (identifier? sym)
         (member (syntax-e sym) '(- not))))
  
  ;; All the symbols we expect to identify
  ;; a binary operation.
  (define (sexp:dyadic? sym)
    (and (identifier? sym)
         (member (syntax-e sym) '(+ - * / \ = < > <= >= <> and or))))
 )