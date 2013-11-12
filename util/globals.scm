(module globals mzscheme
  (provide (all-defined))
  
  (define *gmeta* (make-hash-table))
  (define (add-gmeta sym val)
    (hash-table-put! *gmeta* sym val))
  (define (get-gmeta sym)
    ;; Should this be an error?
    (hash-table-get *gmeta* sym (lambda () #f)))
    
  
  )