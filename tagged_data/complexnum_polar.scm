;; polar representation of complex numbers using magnitude and angle
;; by Alyssa P Hacker

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan x y))))
(define (make-from-mag-ang r a)
  (attach-tag 'polar (cons r a)))
