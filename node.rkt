(module node racket
  
  (provide (except-out (all-defined-out) root))
  
  #| board turn en_passant castle |#
  (struct node (b t e c))
  
  (define (printn n)
    (map displayln (list (pretty n) (node-t n) (node-e n) (node-c n))))
  
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
  
  (define (pretty n)
    (list->string (add-between (string->list (node-b n)) #\space)))
  
  (define root
    (node i_b #t 0 '("wk" "wq" "bk" "bq")))
  
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
  
  (define (make-ally n c)
    (if (node-t n) (char-upcase c) (char-downcase c)))
  
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
    (let ([z (string-ref (node-b n) x)])
      (cond 
        [(equal? z (make-ally n #\P)) (pawn n x)]
        [(equal? z (make-ally n #\N)) (knight n x)]
        [(equal? z (make-ally n #\B)) (brq n x bvec)]
        [(equal? z (make-ally n #\R)) (brq n x rvec)]
        [(equal? z (make-ally n #\Q)) (brq n x qvec)]
        [(equal? z (make-ally n #\K)) (king n x)]
        [else '()]
        )))
  
  (define (gen_all n)
    (apply append 
           (map (curry gen n) (for/list ([x (in-range 0 119)]) x))))
  
  (define (pawn n x)
    (let ([v (if (node-t n) up dn)])
      (append
       (if (empty n (+ x v))
           (if (and
                (empty n (+ x v v))
                (member x (double n)))
               (list (cons x (+ x v)) (cons x (+ x v v)))
               (list (cons x (+ x v))))
           '())
       (if (or
            (foe n (+ x v rt))
            (en_passant n (+ x v rt)))
           (list (cons x (+ x v rt))) '())
       (if (or
            (foe n (+ x v lt))
            (en_passant n (+ x v lt)))
           (list (cons x (+ x v lt))) '()))))
  
  (define (knight n x)
    (for/list ([y nvec]
               #:when (or (foe n (+ x y))
                          (empty n (+ x y))))
      (cons x (+ x y))))
  
  (define (brq n x v)
    (apply append
           (for/list ([y v])
             (for/list ([z (in-range 1 8)]
                        #:break (not (or (foe n (+ x (* z y)))
                                         (empty n (+ x (* z y)))))
                        #:final (foe n (+ x (* z y))))
               (cons x (+ x (* z y)))))))
  
  (define (king n x)
    (append
     (for/list ([y qvec]
                #:when (or (foe n (+ x y))
                           (empty n (+ x y))))
       (cons x (+ x y)))
     (if (and (node-t n) (list? (member "wq" (node-c n))) (equal? (substring (node-b n) 92 95) "..."))
         (list (cons x (- x 2))) '())
     (if (and (node-t n) (list? (member "wk" (node-c n))) (equal? (substring (node-b n) 96 98) ".."))
         (list (cons x (+ x 2))) '())
     (if (and (not (node-t n)) (list? (member "bq" (node-c n))) (equal? (substring (node-b n) 22 25) "..."))
         (list (cons x (- x 2))) '())
     (if (and (not (node-t n)) (list? (member "bk" (node-c n))) (equal? (substring (node-b n) 26 28) ".."))
         (list (cons x (- x 2))) '())))
  
  (define (cas n m r1 r2)
    (list->string
     (map (lambda (x)
            (cond
              [(equal? x (car m)) #\.]
              [(equal? x (cdr m)) #\K]
              [(equal? x r1) #\.]
              [(equal? x r2) #\R]
              [else (string-ref (node-b n) x)]))
          (for/list ([x (in-range 0 119)]) x))))
  
  (define (update n m) 
    (struct-copy
     node n
     [b (cond
          [(equal? m (cons 95 93)) (cas n m 91 94)]
          [(equal? m (cons 95 97)) (cas n m 98 96)]
          [(equal? m (cons 25 23)) (cas n m 21 24)]
          [(equal? m (cons 25 27)) (cas n m 28 26)]
          [else (list->string
                 (map (lambda (x)
                        (if (equal? x (cdr m))
                            (string-ref (node-b n) (car m))
                            (if (equal? x (car m))
                                #\. (string-ref (node-b n) x)))) (for/list ([x (in-range 0 119)]) x)))])]
     [t (not (node-t n))]
     [e (let ([v (if (node-t n) up dn)])
          (if (and
               (member (car m) (double n))
               (equal? (string-ref (node-b n) (car m)) #\P)
               (equal? (cdr m) (+ (car m) v v)))
              (+ (car m) v) 0)
          )]
     [c (case (string-ref (node-b n) (car m))
          [(#\K) (remove* '("wk" "wq") (node-c n))]
          [(#\k) (remove* '("bk" "bq") (node-c n))]
          [(#\R) (if (equal? (car m) 91) (remove "wq" (node-c n)) (remove "wk" (node-c n)))]
          [(#\r) (if (equal? (car m) 21) (remove "bq" (node-c n)) (remove "bk" (node-c n)))]
          [else (node-c n)]
          )]
     ))
  
  
  )