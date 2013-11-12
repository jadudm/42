(module assembly mzscheme
  (require-for-syntax "grammar-helpers.scm")
  (require "../root-structures.scm")
  (provide (all-defined))
  
  (define-values (prop:op has-op? get-op)
                   (make-struct-type-property 'op))
  
  (define-values (prop:asm-sym has-asm-sym? get-asm-sym)
                   (make-struct-type-property 'asm-sym))
  
  ;; (make-asm adc #xF3 arg)
  (define-syntax (define-asm-struct stx)
    (syntax-case stx ()
      [(_ name hex fields ...)
       (let ([type-descriptor (add-prefix (syntax name) 'struct:)]
             [constructor (add-prefix (syntax name) 'make-)]
             [predicate (add-postfix (syntax name) '?)]
             [accessor (add-postfix (syntax name) '-ref)]
             [mutator (add-postfix (syntax name) '-set!)]
             [num-fields (length (syntax->list (syntax (fields ...))))]
             [parent-type (datum->syntax-object
                           (syntax name)
                           (if (< (syntax-e (syntax hex)) 16)
                               'struct:primary
                               'struct:secondary))]
             [accessor-names
              (map (lambda (f)
                     (add-prefix 
                      f 
                      (syntax-object->datum (add-postfix (syntax name) '-)))) 
                   (syntax->list (syntax (fields ...))))]
             [mutator-names
              (map (lambda (f)
                     (add-postfix
                      (add-prefix
                        f
                        (syntax-e
                         (add-prefix 
                          (add-postfix (syntax name) '-) 'set-)))
                      '!)) (syntax->list (syntax (fields ...))))])

         #`(begin
            (begin
              ;; Define the structure type
              (define-values
               (#,type-descriptor #,constructor #,predicate #,accessor #,mutator)
               (make-struct-type (quote name) #,parent-type #,num-fields 0 #f 
                                 (list (cons prop:op hex)
                                       (cons prop:asm-sym (quote name))) 
                                 ;; Currently, no inspector
                                 #;(make-inspector)
                                 ))
              ;; Create named accessors for all of the fields.
              #,@(map (let ([c -1])
                        (lambda (acc)
                          (set! c (add1 c))
                          #`(define (#,acc s) (#,accessor s #,c))))
                      accessor-names)
              ;; Create named mutators for all of the fields.
              #,@(map (let ([c -1])
                        (lambda (mut)
                          (set! c (add1 c))
                          #`(define (#,mut s) (#,mutator s #,c))))
                      mutator-names)
              ))
         )]))
  
  )