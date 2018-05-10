(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
	result
	(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (cont-frac2 n d k)
  (define (contfrac i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (contfrac (+ i 1))))))
  (contfrac 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
