(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(map square (list 1 2 3))
(append (list 1 2 3) (list 4 5 6))
(length (list 4 5 6))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t)))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define tree1 (list 1 (list 2 (list 3 4)) 5))

(count-leaves tree1)


