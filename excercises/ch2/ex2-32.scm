(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest
		(map (lambda (x) (cons (car s) x))
		     rest)))))

(subsets (list 1 2 3))

(display (append (list 0) (map (lambda (x) (cons 3 x))
			       (subsets '()))))

(display (subsets '()))
