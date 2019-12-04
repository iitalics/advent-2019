#lang racket

;; [vectorof real] -> real
(define (manhattan xs)
  (for/sum ([x (in-vector xs)])
    (abs x)))


;; left, right : real
(struct interval [left right] #:transparent)

;; real real -> interval
(define (make-interval l r)
  (interval (min l r) (max l r)))

;; interval real -> boolean
(define (interval-contains? i x)
  (<= (interval-left i) x (interval-right i)))

;; [hash real => [listof interval]]
;; [hash real => [listof interval]]
;;   -> [listof [vector real real]]
(define (interval-intersections x=>y-intervals
                                y=>x-intervals)
  (for*/list ([(x yis) (in-hash x=>y-intervals)]
              [yi (in-list yis)]
              [(y xis) (in-hash y=>x-intervals)]
              #:when (interval-contains? yi y)
              [xi (in-list xis)]
              #:when (interval-contains? xi x))
    (vector x y)))


;; axis : (or 'x 'y)
;; sign : (or -1 +1)
;; len : integer
(struct path [axis sign len] #:transparent)
(define (L len) (path 'x -1 len))
(define (R len) (path 'x +1 len))
(define (U len) (path 'y -1 len))
(define (D len) (path 'y +1 len))

;; path integer integer -> integer integer
(define (path-endpoint p x y)
  (define δ (* (path-len p) (path-sign p)))
  (match (path-axis p)
    ['x (values (+ x δ) y)]
    ['y (values x (+ y δ))]))

;; ... [hash real => [listof interval]] ...
(struct grid [x=>y-intervals y=>x-intervals] #:transparent)

(define (paths->grid paths)
  (define (hash-push h k x)
    (hash-update h k (λ (xs) (cons x xs)) '()))
  (for/fold ([x=>yi (hash)] [y=>xi (hash)]
             [x 0] [y 0]
             #:result (grid x=>yi y=>xi))
            ([p (in-list paths)])
    (define-values [x* y*] (path-endpoint p x y))
    (define-values [x=>yi* y=>xi*]
      (match (path-axis p)
        ['x (values x=>yi (hash-push y=>xi y (make-interval x x*)))]
        ['y (values (hash-push x=>yi x (make-interval y y*)) y=>xi)]))
    (values x=>yi* y=>xi* x* y*)))

(define (grid-intersections g1 g2)
  (append (interval-intersections (grid-x=>y-intervals g1)
                                  (grid-y=>x-intervals g2))
          (interval-intersections (grid-x=>y-intervals g2)
                                  (grid-y=>x-intervals g1))))

(define (read-path [port (current-input-port)])
  (match (regexp-match #px",?([RULD])(\\d+)" port)
    [(list _ dir len)
     ((match dir [#"R" R] [#"L" L] [#"U" U] [#"D" D])
      (string->number (bytes->string/utf-8 len)))]
    [_ eof]))

(define (read-wires [port (current-input-port)])
  (for/list ([l (in-lines port)])
    (with-input-from-string l
      (λ () (for/list ([p (in-port read-path)]) p)))))

(match-define (list (app paths->grid g1) (app paths->grid g2))
  (with-input-from-file "input/day-3.txt" read-wires))

(apply min (for/list ([v (in-list (grid-intersections g1 g2))]
                      #:when (not (equal? v #(0 0))))
             (manhattan v)))
