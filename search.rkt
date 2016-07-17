(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt" "score.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
  (define (make-score-node-pair lst)
    (map (lambda (x) (cons x (score x))) lst))
  
  (define (get-three-candidates lst)
    (map (lambda (x) (car x)) (take (sort (make-score-node-pair lst) #:key cdr >) 3)))

  (define (minimax n)
    (if (node-t n) (curry apply max) (curry apply min)))
  
  (define (spawn d n)
    (if (equal? d 0)
        (score n)
        ((minimax n)
            (map
             (lambda (c) (spawn (- d 1) c))
             (get-three-candidates (map (curry update n) (gen_all n)))))))
  
  )

