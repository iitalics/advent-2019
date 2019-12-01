#lang racket/base

(define (fuel x)
  (- (quotient x 3) 2))

(define (fuel* x)
  (define f (fuel x))
  (if (positive? f)
      (+ f (fuel* f))
      0))

(define (part-1)
  (for/sum ([x (in-port read)])
    (fuel x)))

(define (part-2)
  (for/sum ([x (in-port read)])
    (fuel* x)))

(with-input-from-file "input/day-1.txt" part-1)
(with-input-from-file "input/day-1.txt" part-2)
