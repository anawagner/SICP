;transformations (for picture language picture_language.scm)


;transforms the frame (that the resulting painter is called with)
;and calls original painter in transformed frame
;painter,vector(point),vector(point),vector(point)->painter
;the three points specify origin,edge1,edge2 of new frame
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
					 (make-vect 0.0 0.0)
					 split-point
					 (make-vect 0.0 1.0)))
	  (paint-right (transform-painter painter2
					  split-point
					  (make-vect 1.0 0.0)
					  (make-vect 0.5 1.0))))
      (lambda (frame) (paint-left frame) (paint-right frame)))))

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1 0)
		     (make-vect 0 0)
		     (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1 1)
		     (make-vect 0 1)
		     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0 1)
		     (make-vect 0 0)
		     (make-vect 1 1)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-down (transform-painter painter1
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 0.0)
					 split-point))
	  (paint-up (transform-painter painter2
				       split-point
				       (make-vect 1.0 0.5)
				       (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-down frame)
	(paint-up frame)))))

(define (below1 painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert painter))))
;    (below painter2 painter2)))

(define (split orig-placer split-placer)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split orig-placer split-placer) painter (- n 1))))
	  (orig-placer painter (split-placer smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left up)
	      (bottom-right right)
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) quarter)))
;      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))
(define (wallpaper painter n)
  (let ((combine4 (square-of-four identity identity
				  identity identity)))
    (if (= n 0)
	painter
	(wallpaper (combine4 painter) (- n 1)))))

(define (identity painter) painter)
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
