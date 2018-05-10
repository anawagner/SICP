(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next
			    b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate2 + 0 term a next b))

(define (product term a next b)
  (accumulate2 * 1 term a next b))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(sum square 1 inc 4)
(product square 1 inc 3)
      
