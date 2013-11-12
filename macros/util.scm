(module util mzscheme
  (provide (all-defined))
  
  (define-syntax foreach
  (lambda (stx)
    (syntax-case stx ()
      [(foreach ((lhs rhs) ...) bodies ...)
         #`(begin
            (for-each
             (lambda (lhs ...)
               (begin bodies ...))
             #,@(syntax (rhs ...)))
            )]
      )))
  )