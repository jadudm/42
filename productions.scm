(module productions mzscheme
  
  (require (lib "macros/grammar.scm" "42")) 
  (provide (all-defined))
  
  ;;
  ;; PRODUCTIONS
  ;;
  ;; These are common signatures to all passes. Because we don't
  ;; '(provide (all-defined))' from within a pass, we can get 
  ;; away with using the exact same signatures for all passes.
  ;;
  ;; These should only be used for productions that have one argument.
  (declare-productions (mod process action assignment input output
                                construction seq par loop conditional 
                                specification declaration definition
                                expression literal variable
                                instance
                                chan
                                choice
                                formal
                                name 
                                type
                                err
                                asm
                                ))
  
  #;(mod process definition declaration construction
          specification instance seq par conditional choice
          loop action assignment input output expression literal variable
          name type chan formal)
     
  )