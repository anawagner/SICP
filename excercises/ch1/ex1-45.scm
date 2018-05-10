(define (square n) (* n n))
(define (average a b) (/ (+ a b) 2))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (< n 2)
      f
      (compose f (repeated f (- n 1)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))
(nth-root 4294967296 32)
(nth-root 25 2)
(nth-root 8 3)
