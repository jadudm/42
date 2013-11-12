(module root-structures mzscheme
  (provide (all-defined))
  
  (define-struct node (meta) (make-inspector))
  (define-struct (asm node) ())
  (define-struct (primary asm) (v) (make-inspector))
  (define-struct (secondary asm) ())
  
  )