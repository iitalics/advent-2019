#lang racket

;; nat -> [listof (between 0 9)]
(define (digits x)
  (let loop ([x x] [a '()])
    (if (<= x 10)
        (cons x a)
        (let-values ([(x* d) (quotient/remainder x 10)])
          (loop x* (cons d a))))))

;; [nat -> boolean] -> nat -> boolean
(define ((meets-criteria? len?) n)
  (define digs (digits n))
  (and (apply <= digs)
       (for/or ([g (in-list (group-by values digs))])
         (len? (length g)))))

; [listof integer]
(define passwords
  (match (regexp-match #px"(\\d+)-(\\d+)" (open-input-file "input/day-4.txt"))
    [(list _ lo hi)
     (range (string->number (bytes->string/utf-8 lo))
            (string->number (bytes->string/utf-8 hi)))]))

(count (meets-criteria? (>=/c 2)) passwords)
(count (meets-criteria? (=/c 2)) passwords)
