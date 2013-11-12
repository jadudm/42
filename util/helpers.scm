(module helpers mzscheme
  (require (lib "structures.scm" "42"))
  (provide (all-defined))
  
  (define (lift-in-order ls)
    (cond
      [(null? ls) '()]
      [(assembly? (car ls))
       (append
        (lift-in-order (assembly-instruction* (car ls)))
        (lift-in-order (cdr ls))
        )]
      [else
       (cons (car ls)
             (lift-in-order (cdr ls)))]))
  
  #|
  PREFIX traversal (in-order)
  (append  (lift-in-order (cdr ls))
                    (list (car ls)))
  POSTFIX traversal (r->l)
  (append (list (car ls))
                   (lift-in-order (cdr ls)))
  |#
  
  
  (define (number->literal meta num)
    ;;(debug e2a (printf "number->literal ~a~n" num))
    (make-literal meta num))
  
  (define (snoc ls obj)
    (append
     ls
     (list obj)))
  

  
  (define generate-unique-symbol
    (let ([x 0])
      (lambda (label)
        (set! x (+ x 1)) (string->symbol (format "~a.~a" label x)))))
  
  
  (define (generate-unique-label meta label)
    (make-label meta (generate-unique-symbol label)))

  (define foobar
    (lambda (x) x))
  )