(module expand mzscheme
  (require (lib "structures.scm" "42")
	   (lib "plt-match.ss"))

  (provide expand42)
  
  ;; Make sure we get syntax information for files that get read in.
  (port-count-lines-enabled #t)
  
  (define (s2a3helper syntax-obj)
    (cond
     [(null? syntax-obj) (make-expand-terminal)]
     [(list? (syntax-e syntax-obj))
      `(,syntax-obj
	,@(map s2a3helper (syntax-e syntax-obj)))]
     [(syntax? syntax-obj)
      syntax-obj]
     [else
      (error 'sexp2annotated "Bad bit of stuff: ~a~n" syntax-obj)]))  

  (define (sexp2annotated3 filename path ip)
    (let ([stx (read-syntax filename ip)]) 
      (s2a3helper stx)))

  (define (sexp2annotated2 filename path ip)
    (define (helper syntax-obj)
        (cond
          [(null? syntax-obj) (make-expand-terminal)]
          [(list? (syntax-e syntax-obj))
           `(#;(,filename ,(syntax-line syntax-obj) ,(syntax-column syntax-obj))
               ,syntax-obj
             ,@(map helper (syntax-e syntax-obj)))]
          [(syntax? syntax-obj)
           (syntax-object->datum syntax-obj)]
          [else
           (error 'sexp2annotated "Bad bit of stuff: ~a~n" syntax-obj)]))
    (let ([gathered-stx '()])
      (let loop ([stx (read-syntax filename ip)])
        (unless (eof-object? stx)
          (set! gathered-stx (cons (helper stx) gathered-stx))
          (loop (read-syntax path ip))))
      (reverse gathered-stx)
      ))

 ;; expand-file : <path> -> <expression>
  (define (expand-file path)
    (let-values ([(base name must-be-dir?)
                  (split-path (string->path path))])
      (sexp2annotated3 (path->string name) path (open-input-file path))))

  (define (expand-port src port)
    (let ([stx (read-syntax src port)])
      (if (eof-object? stx)
	  stx
	  (s2a3helper stx))))

  (define expand42
    (match-lambda*
     [`(,(? string? path))
	(expand-file path)]
     [`(,src ,(? port? p))
      (expand-port src p)]
     [`(,(? syntax? stx))
       (s2a3helper stx)]
     ))
  )