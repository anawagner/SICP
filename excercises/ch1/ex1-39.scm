(define (square x) (* x x))

(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
	result
	(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
	x
	(- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

(tan-cf 1.0 10)

(define (cont-frac2 n d k)
  (define (contfrac i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (contfrac (+ i 1))))))
  (contfrac 1))
