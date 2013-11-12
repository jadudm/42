(module parse-errors mzscheme
  (require 
   (prefix list: (lib "list.ss"))
   (lib "util/standard-includes.scm" "42")
   (lib "passes/parsing/parse-helpers.scm" "42"))
  
  (provide (all-defined))
  
  
  ;;
  ;; DECLARATION
  ;;
  ;; The new define-pass macro lets me easily mix in 
  ;; a new unit for an existing signature. Sweet. Therefore,
  ;; I can easily write productions that override
  ;; an existing handler on a per-production basis in the pass.
  (define-override-production (declaration-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     ;; (decl (x))
     ;;        ^
     ;; or
     ;; (decl (x y ...))
     ;;        ^^^^^^^
     ;; Start each error with a pattern match:
     [`(,stx
       ,(? (ssym? 'decl))
       ;; Bad pair or pairs
       ,(? (lambda (ls)
            (let ([first (car ls)]
                  [rest  (cdr ls)])
              (and (syntax? first)
                   (andmap identifier? rest))))
          `(,(? syntax? stx2) ,bad-pair* ...))
       ;; Let's just deal with the pairs...
       ,bodies ...) 
       ;; Here, I've let-bound a variable for later use.
       (let ([the-symbols 
              (list:filter 
               symbol?
               (map (lambda (o)
                      (debug declaration (printf "BP: ~a~n" (syntax-e o)))
                      (if (identifier? o) 
                          (syntax-e o))) bad-pair*))])
         
         ;; But really, all an error handler has to do is throw
         ;; an exception using exn42.
         ;; The syntax is:
         ;; (exn42 type error-symbol msg syntax parent-syntax)
         ;; or
         ;; (exn42 type error-symbol msg syntax)
         ;; if you don't have a parent syntax.
         (exn42 declaration
                bad-declaration-pairs
                (format 
                 (tab-append
                  "Inside a declaration, you typically have pairs of~n"
                  "variable names and types. For example:~n"
                  "\t(decl ([x int]) ...)~n"
                  "You instead wrote:~n"
                  "\t(decl ~a ...)~n"
                  "This is not correct.~n"
                  "In particular, ~a ~a.") 
                 (map syntax-object->datum bad-pair*)
                 (apply string-append
                        (map ->string (list-intersperse the-symbols ", ")))
                 (if (> (length the-symbols) 1)
                     "are not pairs of names and types"
                     "is not a pair of a name and a type"))
                stx2 stx))]
     
     ;; The above error is specific to the case where a declaration
     ;; has nothing but identifiers. This catches whether any
     ;; of the declarations are not lists, and throws a more generic error.
     ;; (decl ([x int] y [z int]))
     ;;        ^^^^^^^^^^^^^^^^^
     [`(,(? syntax? stx)
         ,(? (ssym? 'decl))
         ,(? (lambda (ls)
               (let ([first (car ls)]
                     [rest (cdr ls)])
                 (and (syntax? first)
                      (ormap identifier? rest)
                      (ormap list? rest))))
             `(,(? syntax? stx2) ,bp* ...))
         ,bodies ...) 
       (let* ([typless-var-stx (list:filter identifier? bp*)]
              [typeless-var (map syntax-e typless-var-stx)]
              [num-vars (length typeless-var)])
       (exn42 declaration
              not-all-decl-lists
              (format 
               (tab-append
                "You need to specify variable types and not just names.~n"
                "The variable~a ~a ~a no type definition~a.") 
               (optional-s num-vars)
               (apply string-append 
                      (list-intersperse 
                       (map ->string typeless-var)
                       ", "))
               (if (> num-vars 1) "have" "has")
               (optional-s num-vars))
              stx2 stx))]
     
     ;; (decl ((x int)) (:= x 1) (:= x 1))
     ;;                 ^^^^^^^^^^^^^^^^^
     [`(,(? syntax? stx)
         ,(? (ssym? 'decl))
         ,pairs
         ,bodies ..2)        
       (exn42 declaration
              only-one
              (format 
               (tab-append
                "There should only be one process in a declaration form.~n"
                "Yours contains ~a.") (length bodies))
              stx pstx)]
     
     ;; (decl ([x int])   )
     ;;                ^^^
     [`(,(? syntax? stx)
         ,(? (ssym? 'decl)) 
         ,pairs)        
       (exn42 declaration
              empty-decl
              (format 
               (tab-append
                "You have an empty declaration. It should contain~n"
                "one (and only one) subprocess."))
              stx pstx)]
     
     [else 
      (exn42 declaration extreme-bad (extreme-badness "Declaration") pstx)]))
  
  ;;
  ;; PROCESS
  ;;
  (define-override-production (process-err expr pstx)
    (export err)
    (relies-on)
    (matches
     ;; (decl ((x int)) ((:= x 1) (:= x 1)))
     ;;                 ^^^^^^^^^^^^^^^^^^^
     [`(,(? syntax? stx)
         ,(? list? ls) ...)
       (exn42 process
              not-list
              (format
               (tab-append
                "This looks like a list of processes, but it is neither~n"
                "a 'seq', 'par', or other form."))
              pstx stx)]
     [`(,(? syntax? stx)
         ,(? identifier? name)
         ,(? (lambda (o)
               (and (syntax? o)
                    (integer? (syntax-e o)))) ols)
         ...)
       (define (get-first ls pred?)
         (cond
           [(null? ls) ""]
           [(pred? (car ls)) (car ls)]
           [else (get-first (cdr ls) pred?)]))
       
       (exn42 process literal
              (format
               (tab-append
                "Cannot pass a literal to an instance.~n"
                "In particular, you passed '~a' in '~a'")
               (syntax-e (get-first ols 
                                    (lambda (stx)
                                      (integer? (syntax-e stx)))))
               (syntax-object->datum stx))
              stx pstx)]
     
     [`(,(? syntax? stx) ,rest ...)
       (exn42 process bad "bad process" pstx stx)]
     [else (exn42 process extreme-bad (extreme-badness "Process") pstx)]))
  
  ;;
  ;; TYPE
  ;;
  (define-override-production (type-err expr pstx)
    (export err)
    (relies-on)
    (matches
     ;; (decl ([x "int"]) ...)
     ;;           ^^^^^
     [(? (lambda (o)
           (string? (syntax-e o))) expr)
      ;; The bad type is the 'stx' in this case.
      (exn42 type string-as-type 
             (format
              (tab-append
               "You declared the type ~s, which should have~n"
               "been declared without quotes.")
              (syntax-e expr))
             expr pstx)]
     ;; (decl ([x 3]) ...)
     ;;           ^
     [(? (lambda (o)
           (number? (syntax-e o))) expr)
      ;; The bad type is the 'stx' in this case.
      (exn42 type number-as-type
             (format
              (tab-append
               "You declared the type '~s'... which is a number~n"
               "Really, types should be symbolic... a few examples might~n"
               "be 'int', or 'bool', but not numbers.")
              (syntax-e expr))
             expr pstx)]
     [else (exn42 type extreme-bad (extreme-badness "Type") pstx)]
     ))
  
  ;;
  ;; NAME
  ;;
  (define-override-production (name-err expr pstx)
    (export err)
    (relies-on)
    (matches
     ;; (decl (["x" int]) ...)
     ;;         ^^^
     ;; Only works for names without spaces in them.
     [(? (lambda (o)
           (let ([oexp (syntax-e o)])
             (and (string? oexp)
                  (not (member #\space (string->list oexp)))))) expr)
      (exn42 name string-as-name
             (format
              (tab-append
               "You declared the variable ~a as ~s, which really~n"
               "should have been declared without the quotes.")
              (syntax-e expr)(syntax-e expr))
             expr pstx)]
     
     ;; (decl (["a little story" int]) ...)
     ;;         ^^^^^^^^^^^^^^^^
     [(? (lambda (o) (string? (syntax-e o))) e)
      (exn42 name string-as-name
             (format
              (tab-append
               "You tried to use ~s as a variable name. Really, it~n"
               "looks more like a string. You cannot use strings~n"
               "as variable names, no matter how nice they look.")
              (syntax-e expr))
             e pstx)]
     [else (exn42 name extreme-bad (extreme-badness "Name") pstx)]))
  
  ;;
  ;; VARIABLE
  ;;
  (define-override-production (variable-err expr pstx)
    (export err)
    (relies-on)
    (matches
     ;; (decl ([x int]) ... (:= "x" 3) ...)
     [(? (lambda (o)
           (let ([oexp (syntax-e o)])
             (and (string? oexp)
                  (not (member #\space (string->list oexp))))))
         expr)
      (exn42 variable string-as-variable
             (format
              (tab-append
               "You tried to use ~s as a variable, which if~n"
               "you were trying to use the variable ~a, you should~n"
               "get rid of the quotes.")
              (syntax-e expr) (syntax-e expr))
             expr pstx)]
     
     ;; (decl ([x int]) ... (:= "easy as a b c" 3) ...)
     [(? (lambda (o) (string? (syntax-e o))) expr)
      (exn42 variable string-with-spaces-as-variable
             (format
              (tab-append
               "You tried to use ~s as a variable, which really~n"
               "looks more like a string. You cannot use a string~n"
               "in place of a variable.")
              (syntax-e expr))
             expr pstx)]
     [else (exn42 variable extreme-bad (extreme-badness "Variable") pstx)]))
  
  ;; In adding these, it is obvious some of them can be collapsed
  ;; into one handler. For example, 'seq' and 'par' can
  ;; both be one handler, as well as (possibly) 'input' and 'output'.
  ;; They are separate for some reason in the parser... but
  ;; they really, perhaps, shouldn't be. This might be a holdover from
  ;; some earlier (bad/necessary) design decision.
  ;;
  ;; Collapsing them will make error reporting for them simpler.
  ;; Once collapsed, just a tweak to the 'define-pass' for 'parse'
  ;; will bring things together.
  (define-override-production (formal-err expr pstx)
    (export err)
    (relies-on)
    (matches
     ;; (proc foo([])... - no vars declared there
    ((? (lambda (o) (null? (syntax-e (car o)))) expr)
      (exn42 formal empty-list-in-formal
             (format
              (tab-append
               "You have an empty list in your proc declaration:~n"
               "(~s ~s~ (~s)... n"
               "You should either have something in that list:~n"
               "(proc foo([x int]) ...~n"
               "or nothing: (proc foo() ....~n")
              (syntax-e (car (syntax-e pstx))) (syntax-e (cadr (syntax-e pstx))) (syntax-e (car expr)))
             (car expr) pstx))
     [else 
      (exn42 formal extreme-bad (extreme-badness "Formal") pstx)]))
  
  (define-override-production (definition-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 definition extreme-bad (extreme-badness "Definition") pstx)]))
  
  (define-override-production (construction-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 construction extreme-bad (extreme-badness "Construction") pstx)]))
  
  (define-override-production (specification-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 specification extreme-bad (extreme-badness "Specification") pstx)]))
  
  
  (define-override-production (instance-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 instance extreme-bad (extreme-badness "Instance") pstx)]))
  
  (define-override-production (seq-par-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     ;; In combining the error handlers for both
     ;; 'seq' and 'par', we catch some common cases,
     ;; but also have to be aware that 'par' will have
     ;; concerns that 'seq' does not, and visa versa.
     ;; Not sure how to handle that yet.
     ;;
     [`(,(? syntax? stx)
         ,(? (lambda (o)
               (or ((ssym? 'seq) o)
                   ((ssym? 'par) o))) stxsym))
       (let ([msg (format
                   (tab-append
                    "You have an empty '~a'; there should always~n"
                    "be one or more processes in an '~a'.") 
                   (syntax-e stxsym) 
                   (syntax-e stxsym))])
         (if ((ssym? 'seq) stxsym)
             (exn42 seq empty-seq msg stx pstx)
             (exn42 par empty-par msg stx pstx)))]
     [any
      (exn42 seq extreme-bad (extreme-badness "Seq/Par") pstx)]))
  
  (define-override-production (conditional-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 conditional extreme-bad (extreme-badness "Conditional") pstx)]))
  
  (define-override-production (choice-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 choice extreme-bad (extreme-badness "Choice") pstx)]))
  
  (define-override-production (loop-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 loop extreme-bad (extreme-badness "Loop") pstx)]))
  
  (define-override-production (action-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 action extreme-bad (extreme-badness "Action") pstx)]))
  
  (define-override-production (assignment-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 assignment extreme-bad (extreme-badness "Assignment") pstx)]))
  
  (define-override-production (input-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 input extreme-bad (extreme-badness "Input") pstx)]))
  
  (define-override-production (output-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 output extreme-bad (extreme-badness "output") pstx)]))
  
  (define-override-production (expression-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 expression extreme-bad (extreme-badness "Expression") pstx)]))
  
  (define-override-production (literal-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 literal extreme-bad (extreme-badness "Literal") pstx)]))
  
  (define-override-production (chan-err expr pstx)
    (export err)
    (relies-on)
    (matches 
     [else 
      (exn42 chan extreme-bad (extreme-badness "Chan") pstx)]))
  
  )