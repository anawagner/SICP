(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
;; 3.5.3

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; interleave takes elements alternately from the two streams
(define (intervleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

;; 3.69


(define pythagorean-triples
  (stream-filter (lambda (triple)
		   (= (+ (square (car triple))
			 (square (cadr triple)))
		      (square (caddr triple))))
		 (triples integers integers integers)))

;; 3.70
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

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (let ((w1 (weight s1car))
		 (w2 (weight s2car)))
	     (cond ((< w1 w2)
		    (cons-stream s1car
				 (merge-weighted (stream-cdr s1) s2 weight)))
		   ((> w1 w2)
		    (cons-stream s2car
				 (merge-weighted s1 (stream-cdr s2) weight)))
		   (else
		    (cons-stream
		     s1car
		     (cons-stream s2car
				  (merge-weighted (stream-cdr s1)
						  (stream-cdr s2)

						 weight))))))))))

	       
(define (print-n s n)
  (if (> n 0)
      (begin (display (stream-car s))
             (display ",")
             (print-n (stream-cdr s) (- n 1)))))

(print-n (add-sub-prob 2 1) 10)
