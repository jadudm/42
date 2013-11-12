(module exceptions mzscheme
  (require (lib "plt-match.ss")
           (lib "pretty.ss"))
  (provide (all-defined))
  
  ;; 
  ;; EXCEPTION STRUCTURES
  ;;
  ;; All exceptions in the compiler are currently
  ;; derrived frmo exn:exn42. It has four fields.
  (define-struct exn:exn42 (id msg stx pstx))
  ;; These exception types do not add any fields, and
  ;; although we don't use the different types in any 
  ;; significant way at the moment, it is good to throw
  ;; different types when it is appropriate.
  (define-struct (exn:process exn:exn42) ())
  (define-struct (exn:definition exn:exn42) ())
  (define-struct (exn:construction exn:exn42) ())
  (define-struct (exn:instance exn:exn42) ())
  (define-struct (exn:action exn:exn42) ())
  (define-struct (exn:assignment exn:exn42) ())
  (define-struct (exn:undefined-variable exn:exn42) ())
  (define-struct (exn:variable exn:exn42) ())
  (define-struct (exn:expression exn:exn42) ())
  (define-struct (exn:operand exn:exn42) ())
  (define-struct (exn:declaration exn:exn42) ())
  (define-struct (exn:specification exn:exn42) ())
  (define-struct (exn:seq exn:exn42) ())
  (define-struct (exn:par exn:exn42) ())
  (define-struct (exn:type exn:exn42) ())
  (define-struct (exn:name exn:exn42) ())
  (define-struct (exn:formal exn:exn42) ())
  (define-struct (exn:conditional exn:exn42) ())
  (define-struct (exn:choice exn:exn42) ())
  (define-struct (exn:loop exn:exn42) ())
  (define-struct (exn:input exn:exn42) ())
  (define-struct (exn:output exn:exn42) ())
  (define-struct (exn:literal exn:exn42) ())
  (define-struct (exn:chan exn:exn42) ())
  
  (define-struct (exn:catch-all exn:exn42) ())
  
  (define (internal-compiler-error-string)
    (format 
     (string-append
      "\t**************** INTERNAL COMPILER ERROR ****************~n"
      "\t* End users should never see this bug.                  *~n"
      "\t* End-users are encouraged to report this as a bug.     *~n"
      "\t*********************************************************~n"
      "~n")))
  
  (define (internal-compiler-error str)
    (string-append
     (internal-compiler-error-string)
     str))
     
  ;; macro: exn42
  ;; Two forms:
  ;; (exn42 type id msg stx)
  ;; (exn42 type id msg stx pstx)
  ;; This macro is intended to simplify throwing exceptions in
  ;; the compiler. In particular, it expands to
  ;;
  ;; (raise (make-exn:<type> (quote <type>:<id>) <msg> <stx> <pstx>))
  ;;
  ;; meaning the macro
  ;;
  ;; (exn42 declaration bad-binding "Oops" stx)
  ;;
  ;; will expand to
  ;;
  ;; (raise (make-exn:declaration 'declaration:bad-binding "Oops" stx #f))
  ;;
  ;; Generally, these exceptions should then be handled in handler-exn42, 
  ;; as we have a top-level 'with-handlers' wrapped around the running
  ;; of the compiler. This error handler does little more than format 
  ;; the output provided to the user.
  (define-syntax (exn42 stx)
    
    #;(define (->wiki str type tag)
        #`(string-append
           #,str
           (format
            (string-append
             "~nDiscuss and improve this error!~n"
             "http://www.transterpreter.org/wiki/42/errors/~a:~a~n"
             "~n-------------------------------~n") 
            (quote #,type) (quote #,tag))))
    
    (syntax-case stx ()
      [(_ type id msg stx)
       (let ([exn-maker (datum->syntax-object
                         (syntax str)
                         (string->symbol
                          (format "make-exn:~a" (syntax-object->datum (syntax type))))
                         )]
             [exn-id 
              (datum->syntax-object
               (syntax str)
               (string->symbol
                (format "~a:~a" 
                        (syntax-object->datum (syntax type))
                        (syntax-object->datum (syntax id)))
                         ))]
             #;[msg+ (->wiki (syntax msg)
                           (syntax-e (syntax type))
                           (syntax-e (syntax id)))])
         #`(raise (#,exn-maker (quote #,exn-id) msg stx #f))
         )]
      [(_ type id msg stx pstx)
       (let ([exn-maker (datum->syntax-object
                         (syntax str)
                         (string->symbol
                          (format "make-exn:~a" (syntax-object->datum (syntax type))))
                         )]
             [exn-id 
              (datum->syntax-object
               (syntax str)
               (string->symbol
                (format "~a:~a" 
                        (syntax-object->datum (syntax type))
                        (syntax-object->datum (syntax id)))
                         ))]
            #;[msg+ (->wiki (syntax msg)
                           (syntax-e (syntax type))
                           (syntax-e (syntax id)))])
         #`(raise (#,exn-maker (quote #,exn-id) msg stx pstx))
         )]))

  ;; handle-exn42 :: structure
  ;; PURPOSE
  ;; I don't know what we're going to throw in the way of 
  ;; exceptions in this compiler, but so far they are all
  ;; derrived from one parent, exn:exn42. This parent 
  ;; has four required fields: id, msg, stx, and pstx.
  ;; We can throw errrors, right now, that are optional
  ;; based on the presence of pstx. 
  ;;
  ;; It isn't clear if we need separate handlers for different
  ;; kinds of exceptions or not. So far, there is no
  ;; differentiation, but we're throwing different (child)
  ;; exception types just the same. We may, later,
  ;; come in here and produce different error messages 
  ;; parameterized over the type of error thrown.
  (define (handle-exn42 e)
    (match e
      [(struct exn:exn42 (id msg stx pstx))
       (cond
         [(and (boolean? pstx)
               (not pstx))
          ;;(raise-syntax-error id msg pstx stx)
          (begin
            (printf 
             (string-append
              "~n--> Error ~a <--~n"
              "~nIn file ~a, line ~a, column ~a:~n~n"
              msg
              "~n")
             id 
             (syntax-source stx)
             (syntax-line stx)
             (syntax-column stx))
            (printf "~nOffending source:~n")
            (pretty-print (syntax-object->datum stx))
            (printf "~n")
            (printf
             (string-append
              "~nDiscuss and improve this error!~n"
              "http://www.transterpreter.org/wiki/42/errors/~a~n~n") 
             id)
            (exit))]
         [stx
          (begin
            (printf 
             (string-append
              "~n--> Error ~a <--~n"
              "~nIn file ~a, line ~a, column ~a:~n~n"
              msg
              "~n")
             id 
             (syntax-source stx)
             (syntax-line stx)
             (syntax-column stx))
            (printf "~nOffending source:~n")
            (pretty-print (syntax-object->datum stx))
            (printf "~nEnclosed in:~n")
            (pretty-print (syntax-object->datum pstx))
            (printf "~n")
            (printf
             (string-append
              "~nDiscuss and improve this error!~n"
              "http://www.transterpreter.org/wiki/42/errors/~a~n~n") 
             id)
            (exit))]
         [else (raise-syntax-error id msg stx)])]
      [else
       (error 'handle-error "No idea what to do.")]))
  
  ;; In truth, the extreme-badness function
  ;; should take more parameters, and completely replace the current
  ;; usages. 
  (define (extreme-badness str)
    (format "~n\t** Extreme ~a Badness **~n\tPlease report this to bugs@transterpreter.org!" str))
  
  ;; String maker-helpers
  ;; These are for making pretty syntax error messages.
  
  (define (optional-s n)
    (if (> n 1) "s" ""))
  
  (define (add-s n str)
    (if (> n 1)
        (format "~as")
        str))
        
  (define (->string o) (format "~a" o))
  
  (define (list-intersperse ls obj)
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) ls]
      [else
       (cons (car ls)
             (cons obj
                   (list-intersperse (cdr ls) obj)))]))
  
  
  ;; tab-append :: list-of-strings -> string
  ;; PURPOSE
  ;; Takes a list of strings, and appends a tab to the start
  ;; of each line. Returns a single string by 
  ;; appending all the strings to each-other.
  (define tab-append 
    (lambda str*
      (apply string-append 
             (map (lambda (str)
                    (string-append "\t" str)) str*))))
  
  
  )