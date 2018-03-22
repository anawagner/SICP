;-------------------------
; sets as unordered lists
; ------------------------
(define (ul-element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (ul-element-of-set? x (cdr set)))))

; in unordered list no element appears more than once
; adjoining an element to a set, must check if it already exists
(define (ul-adjoin-set x set)
  (if (ul-element-of-set? x set)
      set
      (cons x set)))

(define (ul-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((ul-element-of-set? (car set1) set2)
	 (cons (car set1) (ul-intersection-set (cdr set1) set2)))
	(else (ul-intersection-set (cdr set1) set2))))

; one quick improvement could be to check wich set is smaller
; and adjoin the smaller set to the larger
(define (ul-union-set set1 set2)
  (if (null? set1)
      set2
      (ul-union-set (cdr set1) (ul-adjoin-set (car set1) set2))))
