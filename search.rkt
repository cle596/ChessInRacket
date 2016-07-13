(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt" "score.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
  (define inf 100000)
  
  (define (spawn d n)
    (if (equal? d 0)
        (list (score n) "none")
        (if (node-t n)
            (apply max
                   (map
                    (lambda (c)
                      (spawn (- d 1) c)
                      )
                    (map (curry update n) (gen_all n))))
            (apply min
                   (map
                    (lambda (c)
                      (spawn (- d 1) c)
                      )
                    (map (curry update n) (gen_all n))))
            )))
    
    )