(define nil '())

(define (square-tree1 mytree)
  (cond ((null? mytree) nil)
	((not (pair? mytree)) (square mytree))
	(else (cons (square-tree (car mytree))
		    (square-tree (cdr mytree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(define (tree-map f tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (f tree))
	(else (cons (tree-map f (car tree))
		    (tree-map f (cdr tree))))))
(define (square-tree tree) (tree-map square tree))

(define mytree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree mytree1)
