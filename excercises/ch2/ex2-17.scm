(define (last-pair items)
  (list-ref items (- (length items) 1)))
(last-pair (list 23 72 149 34))

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25)))
