(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3))
