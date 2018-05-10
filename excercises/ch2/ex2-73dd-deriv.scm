;; exercise 2.73 from 2.4 of SICP

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	                                   var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; sums
(define (install-sum-package)
  ;; internal procedures
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))
  
  (define (deriv-sum s)
    (make-sum (deriv (addend s) var)
	      (deriv (augend s) var)))

  ;; interface to rest of system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))

  'done)
