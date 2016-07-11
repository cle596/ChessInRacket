(module util racket
  
  (require "node.rkt")
  (provide pretty)
  
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
  
  #|
(define root
  (node i_b #t 0 '()))
|#
  )