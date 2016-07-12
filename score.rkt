(module score racket
  
  (provide (except-out (all-defined-out) root))
  
  (require "node.rkt")
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
  (define mat
    (hash
     #\K 20000 #\Q 900
     #\R 500 #\B 330
     #\N 320 #\P 100
     #\k -20000 #\q -900
     #\r -500 #\b -330
     #\n -320 #\p -100))
  
  (define pst
    (hash
     #\K
     '(
       0  0  0  0  0  0  0  0  0  0
          0  0  0  0  0  0  0  0  0  0
          0  0  0  0  0  0  0  0  0  0
          0 50 50 50 50 50 50 50 50  0
          0 10 10 20 30 30 20 10 10  0
          0  5  5 10 25 25 10  5  5  0
          0  0  0  0 20 20  0  0  0  0
          0  5 -5 -10  0  0 -10 -5  5  0
          0  5 10 10 -20 -20 10 10  5  0
          0  0  0  0  0  0  0  0  0  0
          0  0  0  0  0  0  0  0  0  0
          0  0  0  0  0  0  0  0  0  0)
     
     ))
  
  
  )