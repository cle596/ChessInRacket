(module util racket
  
  (require "node.rkt")
  (provide (all-defined-out))
  
  (define (unicfy lst)
    (map (lambda (x)
           (if (hash-has-key? uni x)
               (hash-ref uni x)
               x)) lst))
  
  (define uni
    (hash
     #\K #\u2654 #\Q #\u2655
     #\R #\u2656 #\B #\u2657
     #\N #\u2658 #\P #\u2659
     #\k #\u265A #\q #\u265B
     #\r #\u265C #\b #\u265D
     #\n #\u265E #\p #\u265F))
  
  (define trans_hash
    (hash
     20 #\8 30 #\7
     40 #\6 50 #\5
     60 #\4 70 #\3
     80 #\2 90 #\1
     1 #\a 2 #\b
     3 #\c 4 #\d
     5 #\e 6 #\f
     7 #\g 8 #\h))
  
  (define rtrans_hash
    (hash
     #\8 20 #\7 30
     #\6 40 #\5 50
     #\4 60 #\3 70
     #\2 80 #\1 90
     #\a 1 #\b 2
     #\c 3 #\d 4
     #\e 5 #\f 6
     #\g 7 #\h 8))
  
  (define (pair-to-list x)
    (list (car x) (cdr x)))
  
  (define (trans x)
    (string
     (hash-ref trans_hash (remainder x 10))
     (hash-ref trans_hash (* (quotient x 10) 10))))
  
  (define (ttrans x)
    (apply string-append (map trans (pair-to-list x))))
  
  (define (rtrans x)
    (cons 
    (for/sum ([x (list (hash-ref rtrans_hash (string-ref x 0)) (hash-ref rtrans_hash (string-ref x 1)))]) x)
    (for/sum ([x (list (hash-ref rtrans_hash (string-ref x 2)) (hash-ref rtrans_hash (string-ref x 3)))]) x)))
  
  
  
  
  
  )



