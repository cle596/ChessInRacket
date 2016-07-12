(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
  (define (spawn d n)
    (map
     (lambda (c)
       (if (equal? d 0)
           0
           (begin
             (printn c)
             (read-line)
             (spawn (- d 1) c))))
     (map (curry update n) (gen_all n))))
  
  )