#lang racket

(define (one-of seq) 
  (cond
    [(vector? seq) (vector-ref seq (random (vector-length seq)))]
    [(list? seq) (list-ref seq (random (length seq)))]
    [(string? seq) (one-of (string->list seq))]
    [else (error "one of what?")]))

(define (% threshold) (< (random 100) threshold))
(define (moreless l1 l2 [pc 80]) (if (% pc) l1 l2))

(struct Phon (short unique alts) #:transparent)
(struct Syll (short unique full) #:transparent)
(struct Woid (short unique full) #:transparent)
(define (syllabize f)
  (Syll (apply string-append (map Phon-short f)) (apply string-append (map Phon-unique f)) f))
(define (Syll-alt s)
  (apply string-append
         (map (λ (p) (one-of (cons (Phon-unique p) (Phon-alts p)))) (Syll-full s))))
(define (embwoider s)
  (Woid (apply string-append (map Syll-short s)) (apply string-append (map Syll-unique s)) s))
(define (Woid-alt w)
  (apply string-append (map Syll-alt (Woid-full w))))

(define vowels (list (Phon "a" "a" '()) (Phon "e" "e" '()) (Phon "i" "i" '())
                     (Phon "o" "o" '()) (Phon "u" "u" '()) (Phon "y" "y" '())))

(define vowelish (list (Phon "w" "w" '()) (Phon "r" "r" '()) (Phon "l" "l" '()) 
                       (Phon "m" "m" '()) (Phon "n" "n" '()) (Phon "ŋ" "ng" '("n"))))
(define voif 
  (list (Phon "v" "v" '()) (Phon "ð" "dh" '("th")) (Phon "z" "z" '()) 
        (Phon "ʒ" "zh" '("z" "j")) (Phon "γ" "gh" '("g" "r" "rh"))))
(define unvf
  (list (Phon "f" "f" '("ph")) (Phon "þ" "th" '()) (Phon "s" "s" '("c")) 
        (Phon "ʃ" "sh" '("s" "c")) (Phon "х" "kh" '("x")) (Phon "h" "h" '())))
(define voip
  (list (Phon "b" "b" '()) (Phon "d" "d" '()) (Phon "ծ" "dz" '()) 
        (Phon "ջ" "dzh" '("j" "g")) (Phon "g" "g" '())))
(define unvp
  (list (Phon "p" "p" '()) (Phon "t" "t" '()) (Phon "ц" "ts" '()) (Phon "ч" "ch" '("tc"))
        (Phon "k" "k" '()) (Phon "ﬅ" "st" '())))

(define fricatives (append voif unvf))
(define plosives (append voip unvp))

(define fin (Phon "-" "-" '()))
(define finl (list fin))

(define (orfin p a) (if (% p) a fin))

(define (generate-syllable)
  (syllabize 
   (remove* finl
            (list
             (orfin 30 (if (% 50) (one-of plosives) (one-of fricatives)))
             (orfin 40 (if (% 50) (one-of fricatives) (one-of vowelish)))
             (one-of vowels)
             (orfin 20 (one-of vowels))
             (orfin 40 (if (% 50) (one-of fricatives) (one-of vowelish)))
             (orfin 30 (if (% 50) (one-of plosives) (one-of fricatives)))))))


(define plos-up (append vowels vowelish voif unvf))
(define unvf-up (append vowels vowelish voif))
(define voif-up (append vowels vowelish voif))
(define vish-up (append vowels vowelish))
(define consonants (append vowelish fricatives plosives))
(define vish-dn (append finl unvp voip unvf voif vowelish))
(define voif-dn (append finl unvp voip unvf voif))
(define unvf-dn (append finl unvp voip unvf))

(define all (append vowels vowelish fricatives plosives))

(define (orvow p a) (if (% p) a (one-of vowels)))

(define (gensyl)
  (define (gs sofar vow)
    (let ([fir (first sofar)] [sec (second sofar)])
      (if (member fir vowels) 
          (gs (cons (orfin 20 (one-of (cons fin all))) sofar) #t)
          (if vow
              (cond [(equal? fir fin) (reverse (remove* finl sofar))]
                    [(not (member sec vowels)) (reverse (remove* finl sofar))]
                    [(member fir vowelish) (gs (cons (orfin 20 (one-of vish-dn)) sofar) #t)]
                    [(member fir voif)     (gs (cons (orfin 20 (one-of voif-dn)) sofar) #t)]
                    [(member fir unvf)     (gs (cons (orfin 20 (one-of unvf-dn)) sofar) #t)]
                    [else (reverse (remove* finl sofar))]
                    )
              (cond [(not (member sec (cons fin plosives))) (gs (cons (one-of vowels) sofar) #t)]
                    [(member fir vowelish) (gs (cons (orvow 20 (one-of vish-up)) sofar) #f)]
                    [(member fir voif)     (gs (cons (orvow 20 (one-of voif-up)) sofar) #f)]
                    [(member fir unvf)     (gs (cons (orvow 20 (one-of unvf-up)) sofar) #f)]
                    [else                  (gs (cons (orvow 20 (one-of plos-up)) sofar) #f)]
                    )))))
  (syllabize (gs (list (one-of all) fin) #f)))

(define (word syls [sylgen gensyl])
  (embwoider (map (λ (n) (sylgen)) (build-list syls identity))))
