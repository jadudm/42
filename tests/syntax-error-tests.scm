(module syntax-error-tests mzscheme
  (require 
   (planet "test.ss" ("schematics" "schemeunit.plt" 2 0))
   (lib "exceptions.scm" "42")
   (lib "driver.scm" "42"))
  
  (provide 
   syntax-error:type
   syntax-error:definition
   syntax-error:variable
   syntax-error:instance)
  
  (define-simple-check
   (check-throws-exn42 pred? sym file)
   (with-handlers ([exn:exn42? 
                    (lambda (e)
                      (let ([result
                             (and (pred? e)
                                  (equal? sym (exn:exn42-id e)))])
                        (if (not result)
                            (printf "exn42-id: ~a != ~a~n"
                                    (exn:exn42-id e)
                                    sym))
                        result))])
     (run file)
     ;; If we get this far, it's bad. 
     ;; An exception *should* have been thrown.
     ;; Return false.
     #f
     ))
                 
  (define syntax-error:definition
    (test-suite
     "StxErr: Definition"
     
     (test-case
      "definition:duplicate-variable-name"
      (check-throws-exn42 
       exn:definition? 'definition:duplicate-variable-name
       "cases/syntax/error/definition/duplicate-name-in-formals.soccam"))
     ))
  
  (define syntax-error:variable
    (test-suite
     "StxErr: Variable"
     
     (test-case
      "variable:string-as-variable"
      (check-throws-exn42 
       exn:variable? 'variable:string-as-variable
       "cases/syntax/error/variables/string-as-variable.soccam"))
     
     (test-case
      "variable:string-with-spaces-as-variable"
      (check-throws-exn42 
       exn:variable? 'variable:string-with-spaces-as-variable
       "cases/syntax/error/variables/string-with-spaces-as-variable.soccam"))
     ))
  
  (define syntax-error:instance
    (test-suite
     "StxErr: Instance"
     
     (test-case
      "instance:not-defined"
      (check-throws-exn42 
       exn:instance? 'instance:not-defined
       "cases/syntax/error/instance/instance-undefined.soccam"))
     
     (test-case
      "instance:not-defined"
      (check-throws-exn42 
       exn:instance? 'instance:not-defined
       "cases/syntax/error/instance/instance-undefined2.soccam"))
     ))
  
  ;; Code that throws syntax errors regarding types
  (define syntax-error:type
    (test-suite
     "StxErr: Type"

     (test-case
      "type:string-as-type"
      (check-throws-exn42
       exn:type? 'type:string-as-type
       "cases/syntax/error/types/string-type.soccam"))
     
     (test-case
      "type:number-as-type"
      (check-throws-exn42
       exn:type? 'type:number-as-type
       "cases/syntax/error/types/number-type.soccam"))

     ;; Interesting; we wrote one of these tests some time
     ;; ago, and another when we wrote the pass...
     (test-case
      "type:invalid"
      (check-throws-exn42 
       exn:type? 'type:invalid
       "cases/syntax/error/types/undefined-type.soccam"))
          
     (test-case
      "type:invalid"
      (check-throws-exn42
       exn:type? 'type:invalid
       "cases/syntax/error/types/not-an-int.soccam"))
     
     ))
  )