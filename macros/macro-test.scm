(require "grammar.scm")

(declare-productions (p1 p2 p3 p4 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10))

(define pass-maker
    (make-pass 
     ([p1 b1 p3 b3]
      [p2 b4 b5]
      [p3 b6 p2 b8]
      [p4 b9 p3 b10]
      [b1][b2]
      [b3][b4]
      [b5][b6]
      [b7][b8]
      [b9][b10])))



(define-production (p1 e)
  (relies-on b1 p3 b3)
  (matches
   [any (printf "p1~n") (<p3> any)]))

(define-production (alt-p1 e)
  (export p1)
  (relies-on b1 p3 b3)
  (matches
   [any (printf "alt-p1~n") (<p3> any)]))

;; becomes =>
#;(define p1@ 
    (unit/sig p1^ 
      (import b1^ p3^ b3^)
      (define (<p1> e) (match e (any (printf "p1~n") (<p3> any))))))
  
  (define-production (p2 e)
    (relies-on b4 b5)
    (matches
     [any (printf "p2~n") (<b4> any)]))
  
  (define-production (p3 e)
    (relies-on b6 p2 b8)
    (matches
     [any (printf "p3~n") (<p2> any)]))
  
  (define-production (p4 e)
    (relies-on b9 p3 b10)
    (matches
     [any (printf "p4~n") (<p3> any)]))

  (define-production (new-b2 e)
    (export p3)
    (relies-on b6 p2 b8)
    (matches
     [any any]))
  
  (define-production (b1 e) (relies-on) (matches [any any]))
  (define-production (b2 e) (relies-on) (matches [any any]))
  (define-production (b3 e) (relies-on) (matches [any any]))
  (define-production (b4 e) (relies-on) (matches [any (printf "b4~n") any]))
  (define-production (b5 e) (relies-on) (matches [any any]))
  (define-production (b6 e) (relies-on) (matches [any any]))
  (define-production (b7 e) (relies-on) (matches [any any]))
  (define-production (b8 e) (relies-on) (matches [any any]))
  (define-production (b9 e) (relies-on) (matches [any any]))
  (define-production (b10 e) (relies-on) (matches [any any]))
  

  
  (define-pass foo 
    ([p1 b1 p3 b3]
     [p2 b4 b5]
     [p3 b6 p2 b8]
     [p4 b9 p3 b10]
     [b1][b2]
     [b3][b4]
     [b5][b6]
     [b7][b8]
     [b9][b10]))

  ;; becomes =>
  #;(begin
      (define foo@
        (compound-unit/sig
          (import)
          (link (p2 : p2^ (p2@ b4 b5))
                (p1 : p1^ (p1@ b1 new-b2 b3))
                (p3 : p3^ (p3@ b6 new-b7 b8))
                (p4 : p4^ (p4@ b9 new-b2 b10))
                (new-b7 : p2^ (new-b7@ b4 b5))
                (new-b2 : p3^ (new-b2@ b6 new-b7 b8)))
          (export (var (p1 <p1>)))))
      (define foo (let () (define-values/invoke-unit/sig (<p1>) foo@) <p1>)))
    
 (define bar (pass-maker))
 
 (set! p1@ alt-p1@)
 
 (define gee (pass-maker))
 
    