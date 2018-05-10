(list 1 (list 2 (list 3 4)))

(define items (list 1 3 (list 5 7) 9))

(display items)
(display (car (cdr (car (cdr (cdr items))))))

(define items2 (list (list 7)))

(display items2)
(display (car (car items2)))

(define items3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6
							     7)))))))

(display items3)
(display (car (cdr (car (cdr (car (cdr (car (cdr
				       (car (cdr (car (cdr items3)))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)
