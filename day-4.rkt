#lang racket

;; left, right : real
(struct interval [left right] #:transparent)

;; real real -> interval
(define (make-interval l r)
  (interval (min l r) (max l r)))

(define (in-interval i)
  (in-range (interval-left i)
            (add1 (interval-right i))))

(define (read-interval [port (current-input-port)])
  (match (regexp-match #px"(\\d+)-(\\d+)" port)
    [(list _ lo hi)
     (make-interval (string->number (bytes->string/utf-8 lo))
                    (string->number (bytes->string/utf-8 hi)))]
    [_ eof]))

;; integer -> [listof (between 0 9)]
(define (digits x)
  (let loop ([x x] [a '()])
    (if (<= x 10)
        (cons x a)
        (let-values ([(x* d) (quotient/remainder x 10)])
          (loop x* (cons d a))))))

;; integer -> boolean
(define (meets-criteria? n)
  (let/ec abort
    (for/fold ([prev #f] [rep? #f] #:result rep?)
              ([dig (in-list (digits n))])
      (when (and prev (< dig prev))
        (abort #f))
      (values dig
              (or rep? (equal? dig prev))))))

(for/sum ([n (in-interval (with-input-from-file "input/day-4.txt" read-interval))])
  (if (meets-criteria? n) 1 0))
