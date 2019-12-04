#lang racket

(define (->num s/b)
  (string->number
   (if (bytes? s/b) (bytes->string/utf-8 s/b) s/b)))

(define (read-number port)
  (match (regexp-match #px"\\d+" port)
    [(list x) (->num x)]
    [_ eof]))

(define prog
  (with-input-from-file "input/day-2.txt"
    (λ ()
      (for/vector ([x (in-port read-number)]) x))))

(define (run noun verb)
  (define mem (vector-copy prog))
  (vector-set! mem 1 noun)
  (vector-set! mem 2 verb)

  (define (bin-op f off)
    (vector-set! mem
                 (vector-ref mem (+ off 3))
                 (f (vector-ref mem (vector-ref mem (+ off 1)))
                    (vector-ref mem (vector-ref mem (+ off 2))))))

  (let/ec halt
    (for ([off (in-range 0 (vector-length mem) 4)])
      (match (vector-ref mem off)
        [1 (bin-op + off)]
        [2 (bin-op * off)]
        [99 (halt (vector-ref mem 0))]))))

(run 12 2)

(for*/first ([noun (in-range 100)]
             [verb (in-range 100)]
             #:when (= (run noun verb) 19690720))
  (+ (* 100 noun) verb))
