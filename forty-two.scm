(module forty-two mzscheme
  (require 
   (lib "exceptions.scm" "42")
   (lib "compiler-command-line.scm" "42"))

  (with-handlers ([exn:exn42? handle-exn42])
             (the-line)
             )
  
  (exit))