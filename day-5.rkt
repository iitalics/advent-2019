#lang racket
(require syntax/parse/define
         racket/hash)


(define current-memory
  (make-parameter (vector)))

(define-simple-macro (with-memory m body ...)
  (parameterize ([current-memory m])
    body ...))

(define (mem-rd o) (vector-ref (current-memory) o))
(define (mem-wr o v) (vector-set! (current-memory) o v))



(define current-interaction
  (make-parameter (hash 'input (λ () (error 'input "no input"))
                        'output (λ (v) (error 'output "no output")))))

(define-simple-macro (with-interaction ([k:id f:expr] ...)
                       body ...)
  (parameterize ([current-interaction
                  (hash-union (current-interaction)
                              (make-hash (list (cons 'k f) ...))
                              #:combine (λ (f1 f2) f2))])
    body ...))

(define (input) ((hash-ref (current-interaction) 'input)))
(define (output v) ((hash-ref (current-interaction) 'output) v))

(define (call/simple-i/o #:input in #:output out? f)
  (with-interaction ([input (λ () in)]
                     [output (λ (v) (when (out? v)
                                      (printf "=> ~a\n" v)))])
    (f)))

(define-simple-macro (with-simple-i/o in-expr out?-expr body ...)
  (call/simple-i/o #:input in-expr #:output out?-expr (λ () body ...)))

(struct operand [mode v])

(define (rd opnd)
  (match (operand-mode opnd)
    [0 (mem-rd (operand-v opnd))]
    [1 (operand-v opnd)]))

(define (wr opnd v)
  (match (operand-mode opnd)
    [0 (mem-wr (operand-v opnd) v)]
    [1 (error 'wr "cannot write to immediate mode operand")]))


(struct operator [arity f])
(struct jump [pc])
(struct halt [])

(define operator-table (make-hasheqv))

(define-simple-macro (define-operator (name arg ...) code body)
  (define name
    (let ([op (operator (length '[arg ...]) (λ (arg ...) body))])
      (hash-set! operator-table code op)
      op)))

(define-operator (op:add a b c) 1  (wr c (+ (rd a) (rd b))))
(define-operator (op:mul a b c) 2  (wr c (* (rd a) (rd b))))
(define-operator (op:inp     c) 3  (wr c (input)))
(define-operator (op:out a)     4  (output (rd a)))
(define-operator (op:jnz a b)   5  (unless (zero? (rd a)) (jump (rd b))))
(define-operator (op:jez a b)   6  (when (zero? (rd a)) (jump (rd b))))
(define-operator (op:clt a b c) 7  (wr c (if (< (rd a) (rd b)) 1 0)))
(define-operator (op:ceq a b c) 8  (wr c (if (= (rd a) (rd b)) 1 0)))
(define-operator (op:halt)      99 (halt))

(define (in-digits n #:base [base 10])
  (define-values [n* d] (quotient/remainder n base))
  (stream-cons d (in-digits n* #:base base)))

(define (apply-op optr pc)
  (apply (operator-f optr)
         (for/list ([i (in-range (operator-arity optr))]
                    [mode (in-digits (quotient (mem-rd pc) 100))])
           (operand mode (mem-rd (+ pc 1 i))))))

(define (step pc)
  (define opc (remainder (mem-rd pc) 100))
  (define op (hash-ref operator-table
                       opc
                       (λ () (error 'step (format "invalid opcode ~a" opc)))))
  (match (apply-op op pc)
    [(halt) #f]
    [(jump pc*) pc*]
    [_ (+ pc 1 (operator-arity op))]))

(define (step* [pc0 0])
  (let loop ([pc pc0])
    (when pc
      (loop (step pc)))))

;; -----------------------------------------------------------------------------

(define (read-number [port (current-input-port)])
  (match (regexp-match #px",?(-?\\d+)" port)
    [(list _ x) (string->number (bytes->string/utf-8 x))]
    [_ eof]))

(define (read-program [port (current-input-port)])
  (for/vector ([n (in-port read-number)]) n))

(define prog
  (with-input-from-file "input/day-5.txt" read-program))

(with-memory (vector-copy prog)
  (with-simple-i/o 1 (not/c zero?)
    (step*)))

(with-memory (vector-copy prog)
  (with-simple-i/o 5 (not/c zero?)
    (step*)))
