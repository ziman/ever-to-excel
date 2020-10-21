(define (id x) x)
(define (f x y z) (+ x (- y z)))

(define (main)
  (display
    (f 1 10 100)))
