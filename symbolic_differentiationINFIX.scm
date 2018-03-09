;helper functions
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;gives the correct sub expression from given expression
;either a desired single value out if the list, or the full expression
(define (sub-exp exp)
  (if (null? (cdr exp))
      (car exp)
      exp))

; predicates to differentiate variables
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;compares expression with a number
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;sums 
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s)
  (sub-exp (cddr s)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

;products 
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p)
  (sub-exp (cddr p)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

;exponents
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((and (number? b) (number? e)) (expt b e))
	(else (list b '** e))))

;derive applies mathematical rules for deriving functions
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
				 (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation
			 (base exp)
			 (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
	(else (error "unknown expression type -- DERIV " exp))))

; examples 
(deriv '(x + 3) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x ** 2) 'x)
(deriv '(a * (x ** 2)) 'x)
(deriv '((a * (x ** 2)) + (b * x)) 'x)
(deriv '(((a * (x ** 2)) + (b * x)) + c) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x * y * (x + 3)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
