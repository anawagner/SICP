; huffman trees, SICP 2.3.4


;leaves
;constructor
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;predicate
(define (leaf? object)
  (eq? (car object) 'leaf))
; selectors 
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;code tree
;constructor
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
;selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

; symbols & weight can be called with either a leaf or a tree
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

; decoding procedure takes arguments bits: list of zeroes and ones
;                                    tree: a huffman tree

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOSE-BRANCH" bit))))


; sets of weighted elements, assumes element to be added will never
; already be in the set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)   ;symbol
			       (cadr pair)) ;frequency
		    (make-leaf-set (cdr pairs))))))

; exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
	((member symbol (symbols tree))
	 (let ((left (left-branch tree))
	       (right (right-branch tree)))
	   (if (member symbol (symbols left))
	       (cons 0 (encode-symbol symbol left))
	       (cons 1 (encode-symbol symbol right)))))
	(else (error "bad symbol ENCODE-SYMBOL" symbol))))

(encode (decode sample-message sample-tree) sample-tree)

;exercise 2.69

;takes an ordered set of symbol/frequency pairs (required: no symbol
; appears more than once) generates huffman encoding tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(define somepairs '((A 8) (B 3) (C 1) (D 1) (E 1)
				  (F 1) (G 1) (H 1)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((first (car leaf-set))
	    (second (cadr leaf-set))
	    (rest (cddr leaf-set)))
	(successive-merge (adjoin-set (make-code-tree first second)
				      rest)))))
		 

(generate-huffman-tree somepairs)

;exercise 2.70
(define song-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9)
		     (JOB 2) (WAH 1)))

(define fifties-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1)
					      (SHA 3) (GET 2) (YIP 9)
					      (JOB 2) (WAH 1))))

(define rock-song '(Get a job Sha na na na na na na na na
		    Get a job Sha na na na na na na na na
		    Wah yip yip yip yip yip yip yip yip yip
		    Sha boom))

(display fifties-tree)
(display (make-leaf-set song-pairs))

(define (encode-symbol1 symbol tree)
  (cond ((leaf? tree) '())
	((member symbol (symbols tree))
	 (list 0))
	(else (list 1))))

(encode rock-song fifties-tree)
	
