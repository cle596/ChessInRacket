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

  (define (ally n c)
    (if (node-t n) (char-upper-case? c) (char-lower-case? c)))

  (define (foe n c)
    (not (ally n c)))

  (define (empty n c)
    (if (equal? c #\.) #t #f))
  
  (define (gen n x)
    (case (string-ref (node-b n) x)
      [(#\P #\p) (pawn n x)]
      [(#\N #\n) (knight n x)]
      [(#\R) (- x 10)]
      [else "nothing"]
      ))
  
  (define (pawn n x)
    (let ([v (if (node-t n) up dn)])
    (if (equal? (string-ref (node-b n) (+ x up)) #\.) (list (+ x up)) '()))
  )

  (define (knight n x)
    (map (lambda (y) (+ x y)) nvec))

  )
