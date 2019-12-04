#lang racket
(require racket/hash)

;; left, right : T
(struct interval [left right] #:transparent)

;; [intervalof T] T -> boolean
(define (interval-contains? i x)
  (match-define (interval l r) i)
  (if (< r l)
      (<= r x l)
      (<= l x r)))

;; ux, uy : (or -1 +1)
;; len : integer
(struct path [ux uy len] #:transparent)
(define (L len) (path -1  0 len))
(define (R len) (path +1  0 len))
(define (U len) (path  0 -1 len))
(define (D len) (path  0 +1 len))

;; path -> (or 'h 'v)
(define (path-axis p)
  (if (zero? (path-ux p)) 'v 'h))

;; path integer integer -> integer integer
(define (path-endpoint p x y)
  (values (+ x (* (path-ux p) (path-len p)))
          (+ y (* (path-uy p) (path-len p)))))

;; -> path
(define (read-path [port (current-input-port)])
  (match (regexp-match #px",?([RULD])(\\d+)" port)
    [(list _ dir len)
     ((match dir [#"R" R] [#"L" L] [#"U" U] [#"D" D])
      (string->number (bytes->string/utf-8 len)))]
    [_ eof]))

;; pos : integer
;; int : [intervalof integer]
(struct seg [pos int] #:transparent)

;; [listof seg] [listof seg] -> [listof (cons seg seg)]
(define (seg-intersections segs-1 segs-2)
  (for*/list ([s1 (in-list segs-1)]
              [s2 (in-list segs-2)]
              #:when (and (interval-contains? (seg-int s1) (seg-pos s2))
                          (interval-contains? (seg-int s2) (seg-pos s1))))
    (cons s1 s2)))

;; h-segs, v-segs : [listof seg]
;; pt=>len : [hasheq seg -> nat]
(struct grid [h-segs v-segs seg=>len] #:transparent)

;; grid grid -> [listof (cons seg seg)]
(define (grid-intersections g1 g2)
  (append (seg-intersections (grid-v-segs g1) (grid-h-segs g2))
          (seg-intersections (grid-v-segs g2) (grid-h-segs g1))))

;; [listof path] -> grid
(define (paths->grid paths)
  (for/fold ([vs '()]
             [hs '()]
             [x 0]
             [y 0]
             [seg=>len (hasheq)]
             [len 0]
             #:result (grid vs hs seg=>len))
            ([p (in-list paths)])
    (define-values [x* y*] (path-endpoint p x y))
    (define-values [s vs* hs*]
      (match (path-axis p)
        ['h (let ([s (seg y (interval x x*))]) (values s vs (cons s hs)))]
        ['v (let ([s (seg x (interval y y*))]) (values s (cons s vs) hs))]))
    (values vs* hs* x* y*
            (hash-set seg=>len s len)
            (+ len (path-len p)))))

;; grid seg integer -> nat
(define (wire-len g s pos)
  (match (hash-ref (grid-seg=>len g) s #f)
    [#f 0]
    [len (+ len (abs ; distance to beginning of segment
                 (- pos (interval-left (seg-int s)))))]))

;; g1, g2 : grid
(match-define (list (app paths->grid g1) (app paths->grid g2))
  (with-input-from-file "input/day-3.txt"
    (λ ()
      (for/list ([l (in-lines)])
        (port->list read-path (open-input-string l))))))

(define intersects
  (grid-intersections g1 g2))

(apply min (for/list ([ss (in-list intersects)])
             (match-define (cons (seg x _) (seg y _)) ss)
             (+ (abs x) (abs y))))

(apply min (for/list ([ss (in-list intersects)])
             (match-define (cons s-x s-y) ss)
             (for/sum ([g (list g1 g2)])
               (+ (wire-len g s-x (seg-pos s-y))
                  (wire-len g s-y (seg-pos s-x))))))
