(module check-for-locality-tag mzscheme
  (require (lib "util/standard-includes.scm" "42")
           (lib "passes/identities/identity0.scm" "42"))
  
  (provide check-for-locality-tag)
  
  ;;OVERRIDES
  ;; definition
  ;; declaration
  ;;PURPOSE
  ;; Inserts metadata into variable structures as to whether
  ;; they are local or nonlocal. 
  
  (define cflt-pt
    (copy-production-table identity0-production-table))
  
  (define (no-tag-exn stx pstx)
    (exn42 variable no-locality-tag
           (internal-compiler-error
            (format
             (tab-append
              "The variable '~a' has no tag denoting its locality.  ~n"))
            (syntax-e stx))
           stx pstx))
  
  (define (wrong-locality meta stx pstx)
    (exn42 variable no-locality-tag
           (internal-compiler-error
            (format
             (tab-append
              "The variable '~a' has the locality '~a'.~n"
              "It should be either 'local' or 'nonlocal'"))
            (syntax-e stx)
            (gm meta 'variable-locality))
           stx pstx))
           
  (define-production (variable expr)
    (table cflt-pt)
    (relies-on err)
    (matches
     [(struct variable (meta var))
      (let ([stx (gm meta 'stx)]
            [pstx (gm meta 'pstx)])
        (debug cflt (printf "variable~n"))
        ;; Check that the variable-locality tag is there,
        ;; and check that it is one of the correct values.
        (if (not (member (gm meta 'variable-locality 
                             #:ethunk (lambda ()
                                        (no-tag-exn stx pstx)))
                         '(local nonlocal)))
            (wrong-locality meta stx pstx))
        ;; If everything is OK, recreate the variable structure.
        (make-variable meta var))]
     [err (<err> err)]))
     
  
  (define check-for-locality-tag
    (make-identity0-grammar cflt-pt))
  )