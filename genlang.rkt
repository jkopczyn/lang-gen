#lang racket

; based vaguely on http://www.zompist.com/gen.html

; SELECTION
(define (% threshold) (< (random 100) threshold))

(struct RankedList (dropoff ls lsb) #:transparent)
(define (RL dropoff ls)
  (RankedList dropoff ls ls))
(define (rget rl)
  (let ([df (RankedList-dropoff rl)] [ls (RankedList-ls rl)] [lsb (RankedList-lsb rl)])
    (if (% df)
        (first ls)
        (one-of (if (equal? (length ls) 1) 
                    lsb
                    (RankedList df (rest ls) (if lsb lsb ls)))))))

(define (one-of seq) 
  (cond
    [(vector? seq) (vector-ref seq (random (vector-length seq)))]
    [(list? seq) (list-ref seq (random (length seq)))]
    [(string? seq) (one-of (string->list seq))]
    [(RankedList? seq) (rget seq)]
    [else seq]))

; LANGUAGE DEF
(struct Lang (syl rep raw) #:transparent)

(define Tamadh
  (Lang
   (λ () 
     (let* ([cn (RL 30 '(t k x d b p m n s v r l h f))]
            [vw (RL 30 '(A E I a e i o O y J Y j u w))]
            ; ^Categories^ vSyllable Typesv
            [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   (λ (wrd) 
     (regexp-replaces 
      wrd
      '([#rx"O$" "u"] 
        [#rx"^y" "Y"] [#rx"bb" "b"] [#rx"pp" "p"] [#rx"xx" "x"]
        [#rx"a([AEIaeioOyJYju])" "A\\1"] [#rx"i([AEIaeioOyJYju])" "I\\1"] [#rx"e([AEIaeioOyJYju])" "E\\1"]
        [#rx"a" "â"] [#rx"A" "á"] [#rx"e" "ê"] [#rx"E" "é"] [#rx"i" "î"] [#rx"I" "í"] [#rx"o" "ó"] [#rx"O" "ä"] 
        [#rx"y" "ý"] [#rx"Y" "áì"] [#rx"j" "êì"] [#rx"J" "éì"] [#rx"u" "äu"] 
        [#rx"b" "dh"] [#rx"p" "th"] [#rx"x" "hr"])))
   (λ ([n 3])
     (apply string-append (build-list n (λ (n) ((Lang-syl Tamadh))))))))

(define Ertydon
  (Lang
   (λ ()
     (let* ([c (RL 30 '(r l n m k s p d j f θ t g))]
            [w (RL 30 '(k p d f θ t g))]
            [v (RL 30 '(a i e ei ai o u))]
            
            [syls (RL 30 `((,v) (,v h) (,c ,v) (,c ,v ,c) (,c ,v h) (,v ,c) (,w r ,v) (,w r ,v ,c) (,w r ,v h)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   (λ (wrd)
     (regexp-replaces
      wrd
      '([#rx"θ" "th"] [#px"(.)\\1" "\\1"] [#rx"([^aeo])h" "\\1"])))
   (λ ([n 3]) 
     (apply string-append (build-list n (λ (n) ((Lang-syl Ertydon))))))))

(define Lat
  (Lang
   (λ ()
     (let* ([cn '(b c d f g h j k l m n p q r s t v w x y z 
                    ch sh th ts st)]
            [vw '(a e i o u y)]
            [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   identity
   (λ ([n 3])
     (apply string-append (build-list n (λ (n) ((Lang-syl Lat))))))))

(define Dwarvish
  (Lang
   (λ ()
     (let* ([cn '(d p t k h n m s c v l r b st sh j w f ch g z x)]
            [fcn (RL 15 (append cn '(bh)))]
            [pcn (RL 15 (append cn '(nd)))]
            [vw (RL 30 '(e a i u o ae))]
            [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   (λ (wrd)
     (regexp-replaces
      wrd
      '()))
   (λ ([n 3])
     (apply string-append (build-list n (λ (n) ((Lang-syl Dwarvish))))))))

(define Skif
  (Lang
   (λ ()
     (let* ([cn (RL 15 '(s k f c d t n l r m v h nd sk y st kh sh p b j w g z x))]
            [vw (RL 15 '(e a i u o))]
            [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw ,cn) (,vw)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   (λ (wrd)
     (regexp-replaces
      wrd
      '([#rx"^nd" "d"])))
   (λ ([n 3])
     (apply string-append (build-list n (λ (n) ((Lang-syl Skif))))))))

(define Anavasi
  (Lang
   (λ ()
     (let* ([cn '(n v d t p k m h l r s c b j z)]
            [fcn (RL 15 (append cn '()))]
            [pcn (RL 15 (append cn '()))]
            [vw (RL 30 '(i a e u o ae))]
            [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
       (apply string-append (map symbol->string (map one-of (one-of syls))))))
   (λ (wrd)
     (regexp-replaces
      wrd
      '([#rx"aee" "ae"] [#rx"aae" "ae"])))
   (λ ([n 3])
     (apply string-append (build-list n (λ (n) ((Lang-syl Anavasi))))))))

(define (word lang [wn 4] [wr #t])
  ((Lang-rep lang) ((Lang-raw lang) (if wr (add1 (random wn)) wn))))
(define (name lang [wn 4] [wr #t])
  (string-titlecase (word lang wn wr)))

(define (words lang n [wn 4] [wr #t])
  (build-list n (λ (n) (word lang wn wr))))
(define (names lang n [wn 4] [wr #t])
  (map string-titlecase (words lang n wn wr)))

(define (matching-name lang expr [wn 4] [wr #t])
  (let ([nm (name lang wn wr)])
    (if (regexp-match? expr nm)
        nm
        (matching-name lang expr wn wr))))
(define (matching-names lang expr n [wn 4] [wr #t])
  (build-list n (λ (n) (matching-name lang expr wn wr))))

(define (matches-name lang exprs [wn 4] [wr #t])
  (let ([nm (name lang wn wr)])
    (if (andmap (λ (expr) (regexp-match? expr nm)) exprs)
        nm
        (matches-name lang exprs wn wr))))
(define (matches-names lang exprs n [wn 4] [wr #t])
  (build-list n (λ (n) (matches-name lang exprs wn wr))))

(define (text lang n [wn 4] [wr #t])
  (apply string-append (map (λ (s) (string-append s " ")) (words lang n wn wr))))
