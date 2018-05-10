(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))

((double inc) 5)
(((double double) inc) 5)
(((double (double double)) inc) 5)
