(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (average a b)
  (/ (+ a b) 2))

;; prints the first n elements of a series
(define (print-n s n)
  (if (> n 0)
      (begin (newline)
	     (display (stream-car s))
             (print-n (stream-cdr s) (- n 1)))))
;; 3.5.3

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map (lambda (guess)
				   (sqrt-improve guess x))
				 guesses)))
  guesses)

(print-n (sqrt-stream 2) 10)
