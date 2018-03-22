(load "sets_unordered_lists.scm")
(load "sets_ordered_lists.scm")
(load "sets_binary_trees.scm")

(define tree1 (make-tree 7
			 (make-tree 3
				    (make-tree 1 '() '())
				    (make-tree 5 '() '()))
			 (make-tree 9
				    '()
				    (make-tree 11 '() '()))))
(define tree2
  (make-tree 3
	     (make-tree 1 '() '())
	     (make-tree 7
			(make-tree 5 '() '())
			(make-tree 9
				   '()
				   (make-tree 11 '() '())))))

(define tree3 (make-tree 5
			 (make-tree 3
				    (make-tree 1 '() '())
				    '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))


(tree->list (list->tree '(1 3 5 7 9 11)))

(define evens (list->tree '(0 2 4 6 8 10)))
(define odds (list->tree '(1 3 5 7 9)))
(define primes (list->tree '(2 3 5 7 11 13 17 19)))

(union-set odds evens)
(union-set odds odds)
(intersection-set evens primes)
(intersection-set odds primes)
(intersection-set odds evens)



