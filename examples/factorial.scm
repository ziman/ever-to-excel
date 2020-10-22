(define (fac n)
  (if-zero n
	   1
	   (* n
	      (fac (- n 1)))))

(define (main)
  (display
    (fac 6)))
