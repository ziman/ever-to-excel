(define (fac acc n)
  (if-zero
    n
    acc
    (fac2 (* n acc) (- n 1) 1)))

(define (fac2 acc n diff)
  (if-zero
    n
    acc
    (fac (* n acc) (- n diff))))

(define (main)
  (display
    (fac 1 6)))
