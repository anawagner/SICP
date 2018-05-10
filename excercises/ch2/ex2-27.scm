(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))
(reverse (list 1 4 9 16 25))

(define (fringe x)
  (cond ((null? x) x)
	((not (pair? x)) (list x)) 
	(else (append (fringe (car x)) (fringe (cdr x))))))

(define (deep-reverse x)
  (cond ((null? x) x)
        ((pair? (car x)) (append (deep-reverse (cdr x))
				 (list (deep-reverse (car x)))))
	(else (append (deep-reverse (cdr x))
		      (list (car x))))))

(define x (list (list 1 2) (list 3 4)))
(display x)
(display (reverse x))
(display (deep-reverse x))
(define y (list (list 1 2) (list (list 3 4) (list 5 6 7))))
(display (deep-reverse y))

(fringe x)
(fringe y)
(fringe (list x x))
