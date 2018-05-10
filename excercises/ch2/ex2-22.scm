(define nil '())

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (map-square-list items)
  (map square items))

(map-square-list (list 1 2 3 4 5))


(define (for-each proc items)
  (cond ((null? items) true)
	(else (proc (car items))
	      (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
