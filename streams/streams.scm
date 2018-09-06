;; 3.5 streams

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

;; example
;(stream-car
; (stream-cdr
;  (stream-filter prime? (stream-enumerate-interval 10000 1000000))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

;; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;; 3.51
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;; 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
;; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams integers
					       factorials)))
(stream-ref factorials 9)

;; 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
	       (add-streams (stream-cdr stream) (partial-sums stream))))

;; 3.56
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car (merge (stream-cdr s1)
					    (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

;; 3.59 power series
(define (integrate-series stream)
  (stream-map / stream integers))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1
	       (integrate-series (scale-stream sine-series -1))))

;; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-streams (stream-cdr s1)
					 (stream-cdr s2))
			    (mul-series s1
					s2))))

;; 3.61
(define (invert-unit-series s)
  (cons-stream 1 (mul-series (invert-unit-series x)
			     (scale-stream (stream-cdr s) -1))))
;; 3.62 ?
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))
	       
