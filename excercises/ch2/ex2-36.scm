(define nil '())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define myseqs (list (list 1 2 3) (list 4 5 6)
		     (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 myseqs)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (transpose (map (lambda (col)
		      (matrix-*-vector m col))
		    cols))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
	 m)))

(define mymatrix (list (list 1 2 3 4)
		       (list 4 5 6 6)
		       (list 6 7 8 9)))
(matrix-*-vector mymatrix (list 1 2 3 4))

(dot-product (list 1 2 3) (list 1 2 3))
