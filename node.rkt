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
    (case (string-ref (node-b n) x)
      [(make-ally n #\P) (pawn n x)]
      [(make-ally n #\N) (knight n x)]
      [(make-ally n #\B) (brq n x bvec)]
      [(make-ally n #\R) (brq n x rvec)]
      [(make-ally n #\Q) (brq n x qvec)]
      [(make-ally n #\K) (king n x)]
      [else '()]
      ))
  
  (define (gen_all n)
    (apply append 
           (map (curry gen n) (for/list ([x (in-range 0 119)]) x))))
  
  (define (pawn n x)
    (let ([v (if (node-t n) up dn)])
      (append
       (if (empty n (+ x up))
           (if (and
                (empty n (+ x up up))
                (member x (double n)))
               (list (cons x (+ x up)) (cons x (+ x up up)))
               (list (cons x (+ x up))))
           '())
       (if (or
            (foe n (+ x up rt))
            (en_passant n (+ x up rt)))
           (list (cons x (+ x up rt))) '())
       (if (or
            (foe n (+ x up lt))
            (en_passant n (+ x up lt)))
           (list (cons x (+ x up lt))) '()))))
  
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
     (if (and (node-t n) (member "wq" (node-c n)) (equal? (substring (node-b n) 92 96) "..."))
         '((cons x (- x 2))) '())
     (if (and (node-t n) (member "wk" (node-c n)) (equal? (substring (node-b n) 96 98) ".."))
         '((cons x (+ x 2))) '())
     (if (and (not (node-t n)) (member "bq" (node-c n)) (equal? (substring (node-b n) 22 26) "..."))
         '((cons x (- x 2))) '())
     (if (and (not (node-t n)) (member "bk" (node-c n)) (equal? (substring (node-b n) 26 28) ".."))
         '((cons x (- x 2))) '())))
  
  (define (update n m) 
    (struct-copy
     node n
     [b 
      (list->string
       (map (lambda (x)
              (if (equal? x (cdr m))
                  (string-ref (node-b n) (car m))
                  (if (equal? x (car m))
                      #\. (string-ref (node-b n) x)))) (for/list ([x (in-range 0 119)]) x)))]
     [t (not (node-t n))]
     [e (if (node-t n)
            (if (and
                 (member (car m) (double n))
                 (equal? (string-ref (node-b n) (car m)) #\P)
                 (equal? (cdr m) (+ (car m) up up)))
                (+ (car m) up) 0)
            (if (and
                 (member (car m) (double n))
                 (equal? (string-ref (node-b n) (car m)) #\p)
                 (equal? (cdr m) (+ (car m) dn dn)))
                (+ (car m) dn) 0)
            )]
     [c (case (string-ref (node-b n) (car m))
          [(#\K) (remove* '("wk" "wq") (node-c n))]
          [(#\k) (remove* '("bk" "bq") (node-c n))]
          [(#\R) (if (equal? (car m) 91) (remove "wq" (node-c n)) (remove "wk" (node-c n)))]
          [(#\r) (if (equal? (car m) 21) (remove "bq" (node-c n)) (remove "bk" (node-c n)))]
          )]
     ))
  
  
  )