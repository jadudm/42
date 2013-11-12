(module structures mzscheme
  (require (lib "debug.scm" "42")
           (lib "macros/assembly.scm" "42")
           (lib "root-structures.scm" "42")
           ;; Gives us keyword lambdas.
           (lib "kw.ss"))
  
  (provide (all-defined)
           (all-from (lib "root-structures.scm" "42"))) 
  
  (define-struct sobj (stx datum) (make-inspector))
  
  ;;
  ;; METADATA
  ;; 
  
  ;; The empty metadata structure
  (define (empty-meta) (make-hash-table))
  
  ;; insert-meta! :: node sym any -> void
  ;; Takes a tree node and inserts key/value pairs
  ;; into the metadata. Mutates the structure in place.
  ;;
  ;; key/value lists should look like
  ;; `([k1 v1]
  ;;   [k2 ,v2] ...)  and so on.
  (define (insert-meta! strukt key-value-list)
    (let ([m (node-meta strukt)])
      (for-each 
       (lambda (pair)
         (hash-table-put! m (car pair) (cadr pair)))
       key-value-list)
      (set-node-meta! strukt m)
      strukt
      ))
  ;; insert-meta! can be abbreviated as 'im!'
  (define im! insert-meta!)
  
  ;; get-meta :: node sym -> any
  ;; Takes a tree node and returns a value from the node's
  ;; metadata. Takes an optional error handler accessed via
  ;; the keyword #:ethunk. For example
  ;; 
  ;; (get-meta foo 'x) will extract the value associated with 'x 
  ;;                   from this structure's metadata.
  ;;
  ;; (get-meta foo 'y #:ethunk (lambda () 0))
  ;;                   This will extract the value associated with 'y,
  ;;                   but if that isn't found, it will return 0 instead
  ;;                   of throwing an error (the default response).
  (define/kw (extract-and-get-meta strukt strukt-key 
                                   #:key  [ethunk 
                                           (lambda () 
                                             (error 'get-meta "No metadata: ~a" strukt-key))])
             (let ([m (node-meta strukt)])
               (hash-table-get m strukt-key ethunk)))
  (define e&gm extract-and-get-meta)
  
  ;; get-meta can be abbreviated as 'gm'
  (define/kw (get-meta hash key
                       #:key  [ethunk 
                               (lambda () 
                                 (error 'get-meta "No metadata: ~a" key))])
             (hash-table-get hash key ethunk))
  
  
  (define gm get-meta)
  
  ;; We could define get-meta this way.
  #|
  (define get-meta
    (case-lambda 
      [(strukt key) 
       (get-meta strukt key
                 (lambda () 
                   (error 'get-meta "No metadata")))]
      
      [(strukt key ethunk) 
       (let ([m (node-meta strukt)])
         (hash-table-get m key ethunk))
       ]))

  ;; And then we would invoke it differently
  ;;  With keywords:
  ;;   (get-meta node 'stx #:ethunk (lambda () 0))
  ;;  Without:
  ;;   (get-meta node 'stx (lambda () 0))
  |#
  
  
  ;; 
  ;; STRUCTURES
  ;;
  
  (define-struct expand-terminal ())
  
  
  ;; We'll be using this a lot..
  (define NM node-meta)
  
  (define-struct (mod node) (process+) (make-inspector))
  
  ;; Process structures
  (define-struct (process node) () (make-inspector))
  (define-struct (stop process) () (make-inspector))
  (define-struct (skip process) () (make-inspector))
  (define-struct (action process) () (make-inspector))
  
  ;; Action structures
  (define-struct (assignment action) (var exp) (make-inspector))
  (define-struct (input action) (chan var) (make-inspector))
  (define-struct (output action) (chan exp) (make-inspector))
  
  ;; Specification structures
  (define-struct (specification process) () (make-inspector))
  (define-struct (declaration specification) (name-type+ process) (make-inspector))
  (define-struct (definition specification) (name formal* process) (make-inspector))
  
  ;; Construction structures
  (define-struct (construction process) () (make-inspector))
  (define-struct (seq construction) (process+) (make-inspector))
  (define-struct (par construction) (process+) (make-inspector))
  (define-struct (conditional construction) (choice+) (make-inspector))
  (define-struct (instance process) (name actual*) (make-inspector))
  
  ;; Looping structures
  (define-struct (loop construction) () (make-inspector))
  (define-struct (while loop) (exp process) (make-inspector))
  
  ;; Expression structures
  (define-struct (expression node) () (make-inspector))
  (define-struct (monadic-expression expression) (rator rand) (make-inspector))
  (define-struct (dyadic-expression expression) (rator rand1 rand2) (make-inspector))
  
  ;; Atomic structures
  (define-struct (atomic node) () (make-inspector))
  (define-struct (variable atomic) (sym) (make-inspector))
  (define-struct (chan atomic) (sym) (make-inspector))
  (define-struct (literal atomic) (num) (make-inspector))
  (define-struct (name atomic) (sym) (make-inspector))
  (define-struct (type atomic) (sym) (make-inspector))
  
  (define-struct (formal node) (name type) (make-inspector))
  (define-struct (choice node) (exp process) (make-inspector))
  
  ;;
  ;; ASM
  ;;
  ;; (make-asm adc #xF3 arg)
  (define-struct (assembly node) (instruction*) (make-inspector))
  
  (define (flatten ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls)) (append (flatten (car ls)) (flatten (cdr ls)))]
      [(not (list? (car ls))) (cons (car ls) (flatten (cdr ls)))]))
  
  (define make-assembly*
    (lambda ls
      (make-assembly (car ls)
                     (flatten (cdr ls)))))
  
  
  ;; We'll need this some day...
  #;(define (count-bytes n)
      (let ([r (arithmetic-shift n -8)])
        (if (zero? r)
            1
            (add1 (count-bytes r)))))
  
  ;; Label structure
  (define-asm-struct label     -1)
  (define-asm-struct comment   -2)
  
  ;; Primary instructions
  ;; These will inherit from struct:primary, and therefore
  ;; gain 'meta' automatically (from struct:node), and one field, 'v',
  ;; from struct:primary.
  (define-asm-struct j       #x00)
  (define-asm-struct ldlp    #x01)
  (define-asm-struct pfix    #x02)
  (define-asm-struct ldnl    #x03)
  (define-asm-struct ldc     #x04) 
  (define-asm-struct ldnlp   #x05)
  (define-asm-struct nfix    #x06)
  (define-asm-struct ldl     #x07)
  (define-asm-struct adc     #x08)
  (define-asm-struct call    #x09)
  (define-asm-struct cj      #x0A)
  (define-asm-struct ajw     #x0B)
  (define-asm-struct eqc     #x0C)
  (define-asm-struct stl     #x0D)
  (define-asm-struct stnl    #x0E)
  (define-asm-struct opr     #x0F)
  
  ;; Secondary instructions
  ;; These will inherit from struct:secondary, and therefore
  ;; only have a metadata field. 
  (define-asm-struct rev     #xF0)
  (define-asm-struct lb      #xF1)
  (define-asm-struct bsub    #xF2)
  (define-asm-struct endp    #xF3)
  (define-asm-struct diff    #xF4)
  (define-asm-struct add     #xF5)
  (define-asm-struct gcall   #xF6)
  (define-asm-struct in      #xF7)
  (define-asm-struct prod    #xF8)
  (define-asm-struct gt      #xF9)
  (define-asm-struct wsub    #xFA)
  (define-asm-struct out     #xFB)
  (define-asm-struct sub     #xFC)
  (define-asm-struct startp  #xFD)
  (define-asm-struct outbyte #xFE)
  (define-asm-struct outword #xFF)
  
  (define-asm-struct not     #x23F2)
  
  )