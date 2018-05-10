					; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
					; iterative

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

					; factorial

(define (even? x) (= (remainder x 2) 0))

(define (factorial n)
  (define (inc n) (+ n 1))
  (define (identity n) n)

  (product2 identity 1 inc n))
					; test factorial

(factorial 5)
(factorial 9)


(define (approx-pi n)
  (define (inc n) (+ n 1))

  (define (term k)
    (if (even? k)
	(/ (+ k 2) (+ k 1))
	(/ (+ k 1) (+ k 2))))
  
  (* 4 (product2 term 1.0 inc n)))

(approx-pi 1000000)

