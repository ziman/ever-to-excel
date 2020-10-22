(define (fac acc n)
  (if-zero
    n
    acc
    (fac (* n acc) (- n 1))))

(define (main)
  (display
    (fac 1 6)))
