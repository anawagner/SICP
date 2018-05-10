(define (inc n) (+ n 1))
(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (if (< n 2)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)
((repeated inc 2) 5)
((repeated inc 10) 10)

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (smooth-nfold f n)
  ((repeated smooth n) f))

