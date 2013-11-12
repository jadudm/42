(module test-maker mzscheme
  
  (require (lib "process.ss")
           (lib "cmdline.ss"))
  
  (command-line
   "test-maker"
   (current-command-line-arguments)
   (args
    (filename)
    (let-values ([(base name foo)
                  (split-path filename)])
      (let ([name.soccam
             (format "~a.soccam" name)])
        (let ([op (open-output-file (format "~a/~a" base name.soccam))])
          (fprintf 
           op
           ";; ~a~n;; Tests: ~n(module ~a soccam~n~n)~n"
           name.soccam
           name)
          (close-output-port op))
        
        (if (equal? (system-type 'os) 'macosx)
            (system (format "open ~a/~a" base name.soccam)))
        (exit)
        ))))
  )


