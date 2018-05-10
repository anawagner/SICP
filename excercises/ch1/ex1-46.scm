(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (iterative-improve good? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
	(if (good? guess next)
	  next
	  (try next))))
    (try first-guess)))

(define (fixed-point2 f first-guess)
  (define (improve guess)
    (f guess))
  ((iterative-improve close-enough? improve) first-guess))

(fixed-point cos 1.0)
(fixed-point2 cos 1.0)

(define (sqrt2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) 1.0))

(sqrt 25)
(sqrt2 25)
