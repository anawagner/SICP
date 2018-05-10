(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (next x)
	(if (= x 2) 3 (+ x 2)))
      (define (divides? a b)
	(= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
	    ((divides? test-divisor n) test-divisor)
	    (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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

(define (filtered-acumulate2 combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((filter a) (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (iter a null-value))

(define (filtered-acumulate combiner filter null-value term a next b)
  (cond ((> a b) null-value)
	((filter a) (combiner (term a)
			      (filtered-acumulate combiner filter
						  null-value term
						  (next a)
						  next b)))
	(else (filtered-acumulate combiner filter null-value term
				  (next a) next b))))
	
(define (sum term a next b)
  (accumulate2 + 0 term a next b))

(define (product term a next b)
  (accumulate2 * 1 term a next b))

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (identity x) x))

(sum square 1 inc 4)
(product square 1 inc 3)

(define (sum-sq-primes a b)
  (filtered-acumulate + prime? 0 square a inc b))

(sum-sq-primes 2 10)

(define (prod-coprimes n)
  (define (coprime? i)
    (= (gcd i n) 1))
  (filtered-acumulate * coprime? 1 identity 1 inc (- n 1)))

(prod-coprimes 10)

      
