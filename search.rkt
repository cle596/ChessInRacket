(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt" "score.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
  (define inf 100000)
  
  (define (spawn d n)
    (let ([v (if (node-t n) (vector (* -1 inf)) (vector inf))])
      (if (equal? d 0)
          (score n)
          (map
           (lambda (c)
             (begin
               (printn c d (vector-ref v 0))
               (read-line)
               (letrec ([s (spawn (- d 1) c)]
                        [m (if (node-t n) (max s (vector-ref v 0)) (min s (vector-ref v 0)))])
                 (vector-set! v 0 m)
                 m
                 )))
           (map (curry update n) (gen_all n))))))
  
  )