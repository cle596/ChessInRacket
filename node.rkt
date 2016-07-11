(module node racket
  (provide (except-out (all-defined-out) root))
  
  #| board turn en_passant castle |#
  (struct node (b t e c))
  
  (define i_b
    (string-append
     "         \n"
     "         \n"
     " rnbqkbnr\n"
     " pppppppp\n"
     " ........\n"
     " ........\n"
     " ........\n"
     " ........\n"
     " PPPPPPPP\n"
     " RNBQKBNR\n"
     "         \n"
     "         \n"))
  
  (define f_c '("wk" "wq" "bk" "bq"))
  
  (define root
    (node i_b #t 0 '()))
  
  (define up -10)
  (define dn 10)
  (define rt 1)
  (define lt -1)
  
  (define nvec (list
                (+ up up rt) (+ up rt rt)
                (+ dn dn rt) (+ dn rt rt) 
                (+ up up lt) (+ up lt lt)
                (+ dn dn lt) (+ dn lt lt)                
                ))
  
  (define bvec (list
                (+ up rt) (+ up lt)
                (+ dn rt) (+ dn lt)              
                ))
  
  (define rvec (list
                up dn
                rt lt
                ))
  
  (define qvec (append bvec rvec))
  
  (define (ally n x)
    (let ([c (string-ref (node-b n) x)])
      (if (node-t n) (char-upper-case? c) (char-lower-case? c))))
  
  (define (foe n x)
    (let ([c (string-ref (node-b n) x)])
      (if (node-t n) (char-lower-case? c) (char-upper-case? c))))
  
  (define (empty n x)
    (if (equal? (string-ref (node-b n) x) #\.) #t #f))
  
  (define (en_passant n x)
    (if (equal? (node-e n) x) #t #f))
  
  (define (double n)
    (let ([y (if (node-t n) 81 31)])
      (for/list ([x (in-range y (+ y 8))]) x)))
  
  (define (gen n x)
    (case (string-ref (node-b n) x)
      [(#\P #\p) (pawn n x)]
      [(#\N #\n) (knight n x)]
      [(#\B #\b #\R #\r #\Q #\q) (brq n x)]
      [(#\K #\k) (king n x)]
      [else "nothing"]
      ))
  
  (define (pawn n x)
    (let ([v (if (node-t n) up dn)])
      (append
       (if (empty n (+ x up))
           (if (and
                (empty n (+ x up up))
                (member x (double n)))
               (list (+ x up) (+ x up up))
               (list (+ x up)))
           '())
       (if (or
            (foe n (+ x up rt))
            (en_passant n (+ x up rt)))
           (list (+ x up rt)) '())
       (if (or
            (foe n (+ x up lt))
            (en_passant n (+ x up lt)))
           (list (+ x up lt)) '()))))
  
  (define (knight n x)
    (for/list ([y nvec]
               #:when (or (foe n (+ x y))
                          (empty n (+ x y))))
      (+ x y)))
  
  (define (brq n x)
    (apply append
           (for/list ([y bvec])
             (for/list ([z (in-range 1 8)]
                        #:break (not (or (foe n (+ x (* z y)))
                                         (empty n (+ x (* z y)))))
                        #:final (foe n (+ x (* z y))))
               (+ x (* z y))))))
  
  (define (king n x)
    (for/list ([y qvec]
               #:when (or (foe n (+ x y))
                          (empty n (+ x y))))
      (+ x y)))
  
  )
