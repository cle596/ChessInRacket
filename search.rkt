(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt" "score.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq") (cons 0 0) 0))
  
  (define (make-score-node-pair lst)
    (map (lambda (x) (cons x (score x))) lst))
  
  (define (get-three-candidates n lst)
    (if (node-t n)
    (map (lambda (x) (car x)) (take (sort (make-score-node-pair lst) #:key cdr >) 3))
    (map (lambda (x) (car x)) (take (sort (make-score-node-pair lst) #:key cdr <) 3))))
  
  (define (minimax n lst)
    (car
     (if (node-t n)
         (sort lst #:key cdr >)
         (sort lst #:key cdr <))))
  
  (define (spawn d n)
    (if (equal? d 0)
        (cons (node-m n) (score n))
        (minimax n
                 (map
                  (lambda (c) (spawn (- d 1) c))
                  (get-three-candidates n (map (curry update n) (gen_all n)))))))
  
  )

