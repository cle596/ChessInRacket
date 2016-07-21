(module search racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt" "score.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq") (cons 0 0) 0))
  
  (define (make-score-node-pair lst)
    (map (lambda (x) (cons x (score x))) lst))
  
  (define (take-upto lst n)
    (if (>= (length lst) 3)
        (take lst 3)
        (take lst n)))
  
  (define (get-three-candidates n lst)
    (if (node-t n)
        (map (lambda (x) (car x)) (take-upto (sort (make-score-node-pair lst) #:key cdr >) 3))
        (map (lambda (x) (car x)) (take-upto (sort (make-score-node-pair lst) #:key cdr <) 3))))
  
  (define (minimax n lst)
    (letrec ([m
              (if (node-t n)
                  (sort lst #:key (compose cdr cdr) >)
                  (sort lst #:key (compose cdr cdr) <))]
             [mm (car m)])
      (cons (car mm) (cdr (cdr mm)))))
  
  (define (spawn d n)
    (if (equal? d 0)
        (cons (node-m n) (score n))
        (minimax n (for/list ([t (get-three-candidates n (for/list ([m (gen_all n)]) (update n m)))])
                     (cons (node-m t) (spawn (- d 1) t))))))
  
  )

