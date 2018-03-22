; ------
; sets as ordered lists
; -----

(define (ol-element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (ol-element-of-set? x (cdr set)))))

(define (ol-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2) (cons x1 (ol-intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2) (ol-intersection-set (cdr set1) set2))
	      ((< x2 x1) (ol-intersection-set set1 (cdr set2)))))))

(define (ol-adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
	((= x (car set)) set)
	(else (cons (car set) (ol-adjoin-set x (cdr set))))))

(define (ol-union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2) (cons x1 (ol-union-set (cdr set1) (cdr set2))))
		 ((< x1 x2) (cons x1 (ol-union-set (cdr set1) set2)))
		 ((< x2 x1) (cons x2 (ol-union-set set1 (cdr set2)))))))))
