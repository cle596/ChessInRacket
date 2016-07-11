(module input racket
  
  (provide (all-defined-out))
  
  (define iton
    (hash
     #\a 1 #\b 2
     #\c 3 #\d 4
     #\e 5 #\f 6
     #\g 7 #\h 8
     #\1 90 #\2 80
     #\3 70 #\4 60
     #\5 50 #\6 40
     #\7 30 #\8 20))
  
  (define ntoi
    (hash
     1 #\a 2 #\b
     3 #\c 4 #\d
     5 #\e 6 #\f
     7 #\g 8 #\h
     90 #\1 80 #\2
     70 #\3 60 #\4
     50 #\5 40 #\6
     30 #\7 20 #\8))
  
  (define (in i)
    (cons (+ (hash-ref iton (string-ref i 0))
             (hash-ref iton (string-ref i 1)))
          (+ (hash-ref iton (string-ref i 2))
             (hash-ref iton (string-ref i 3)))))
  
  (define (ni n)
    (string
     (hash-ref ntoi (remainder (car n) 10))
     (hash-ref ntoi (* 10 (quotient (car n) 10)))
     (hash-ref ntoi (remainder (cdr n) 10))
     (hash-ref ntoi (* 10 (quotient (cdr n) 10)))
     ))
  
  )