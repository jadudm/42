(module compiler-command-line mzscheme
  (require 
   (prefix standard: (lib "driver.scm" "42"))
   (prefix efficienC: (lib "efficienC-driver.scm" "42"))
   (prefix bytecode: (lib "bytecode-driver.scm" "42"))
   (lib "util/standard-includes.scm" "42")
   (lib "passes/parsing/expand.scm" "42")
   (lib "cmdline.ss"))
  
  (provide (all-defined))
  
  ;; Assuming we import (lib "driver.scm" "42")
  (define *run* standard:run)
  
  (define (the-line)
    (command-line 
     "42"
     (current-command-line-arguments)
     (once-each
      [("--graphviz")
       directory
       "Creates graphviz files for each process~n"
       "in the named directory. The directory is created if needed;"
       "files are overwritten."
       (add-debug-flag! 'silent:graphviz)
       (add-gmeta 'graphviz-directory directory)]
      [("--efficienC")
       "Enable output of native code"
       "(an efficient C representation of the 42 program)"
       (set! *run* efficienC:run)]
      [("--bytecode")
       "Same as the default driver, but... we didn't want to change that."
       "Should output executable bytecode."
       (set! *run* bytecode:run)]
      )
     
     (multi
      [("--disable")
       flag
       "Disable one or more features of the compiler."
       (add-disable-flag! flag)]
      [("-d" "--debug")
       flag
       "A debug flag to pass to the compiler"
       (add-debug-flag! flag)])
     
     ;; I moved the 'with-handlers' form into 
     ;; "forty-two.scm", which seemed like a better place.
     ;; This way, we just run the driver here. Someone
     ;; else can worry about error handling.
     (args (filename)
           (*run* filename)
           )))
  )