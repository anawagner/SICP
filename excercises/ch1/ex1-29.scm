(define (even? x) (= (remainder x 2) 0))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((even? k) 2)
	     ((or (= k 0) (= k n)) 1)
	     (else 4))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

(simpson cube 0 1 100.0)
    
