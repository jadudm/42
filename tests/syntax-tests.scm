(module syntax-tests mzscheme 
  (provide (all-defined))
;; These tests are syntactically correct.
;; They may fail other correctness checks
  
  (define (make-syntax-test-cases)
    (map (lambda (exp)
           `(make-test-case
             (let ([exp (quote ,exp)])
               (format "~a" exp)
               (assert-pred struct? 
                            (first exp)
                            (format "Failed: ~a" exp)))))
         (syntactically-correct)))
           
  (define (syntactically-correct)
  `(
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Correct proc declarations ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (proc foo() (skip))
    (proc foo ([x int]) (skip))
    (proc foo ([x chan]) (skip))
    (proc foo ([x int][y chan]) (skip))
    (proc foo ([y chan][x int]) (skip))
    (proc foo ([z chan][x chan]) (skip))
    (proc foo ([z int][x int]) (skip))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Correct decl blocks ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    (proc foo () (decl ([y int]) (:= y 3)))
    (proc foo () (decl ([y chan]) (:= y 3)))
    (proc foo () (decl ([x int]
                        [y int]) 
                   (par
                    (:= y 3)
                    (:= x 5)
                   )))
    (proc foo () (decl ([x chan]
                        [y int])
                   (! x y)))
    (proc foo () (decl ([x chan]
                        [y chan])
                   (seq
                    (? x y)
                    (! y x)
                   )))
    (proc foo () (decl ([x int]
                        [y chan])
                   (? y x)
                   ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Seq, par skip and stop ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (proc foo ()  
          (seq (skip)))
    (proc foo ()  
          (seq (stop)))
    (proc foo ()  
          (par (skip)))
    (proc foo ()  
          (par (stop)))
    (proc foo()
          (skip))
    (proc foo()
          (stop))
    ;;seq in par with decls
    ;;Bad par useage on Y
    (proc foo () (decl ([x int]
                        [y int])
                   (seq 
                    (:= x 3) 
                    (par 
                     (:= y 5) 
                     (:= y 5)))))
    ;;seq with instance
    (proc foo () 
          (decl ([x int]
                 [y int])
            (seq
             ;; Woot. An instance.
             (foo x y)
             (:= x 3))))
    ;;par with three instances
    (proc foo()
          (decl 
            ([a int] 
             [b int] 
             [c int])
            (par
             (boo a)
             (coo b)
             (doo c))))
    ;;seq with three instances
    (proc foo()
          (decl 
            ([a int] 
             [b int] 
             [c int])
            (seq
             (boo a)
             (coo b)
             (doo c))))
    
    
    ))

;;These tests should cause the parser to fail
(define (syntactically-incorrect)
  `(
    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Empty Par and seq ;;
    ;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Proc with empty seq 
    (proc foo () (decl (x int) (seq)))
    ;; Proc with empty par 
    (proc foo () (decl (x int) (par)))
    ;; par with empty seq
    (proc foo () (decl (x int) (par (seq))))
    ;; par with empty par
    (proc foo () (decl (x int) (par (par ))))
    ;; seq with empty par
    (proc foo () (decl (x int) (seq (par))))
    ;; seq with empty seq
    (proc foo () (decl (x int) (seq (seq))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Broken declarations ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; declaration on its own
    (decl (x int))
    ;; incorrect declaration on its own
    (decl (3 int))
    ;;incorrect declarations
    (proc foo() (decl (x)))
    (proc foo() (decl (int)))
    (proc foo() (decl (3 int)))
    (proc foo() (decl (x 3)))
    (proc foo() (decl (x y)))
    (proc foo() (decl (x int x)))
    (proc foo() (decl (x (int x))))
    
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; Broken proc defs ;;
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; Bad headers
    (proc foo(x) (skip))
    (proc foo(int) (skip))
    (proc foo(x y) (skip))
    (proc foo(int x)(skip))
    (proc foo([x int][x y]) (skip))
    ;;Missing process
    (proc foo([x int])) 
    (proc foo())
    ;; throws a bad process error
    (proc foo () x))
    ))
)