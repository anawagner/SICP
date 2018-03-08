;; a picture language library
(define nil '())
;takes two functions as agruments and returns the composed function
;first function applied to result of second function
(define (compose f g)
  (lambda (x) (f (g x))))


;for-each (procedure, list -> true)
; side effect: proc is applied to each item in list
(define (for-each proc items)
  (cond ((null? items) #t)
	(else (proc (car items)) (for-each proc (cdr items)))))
	    
;scales images to fit frame, takes frame as argument, returns function
;returned function takes a vector to be mapped to frame and returns
;vector sum: origin(frame) + x*edge1(frame) + y*edge1(frame)
;input vector must be in unit square (0<=x,y<=1)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
	       (scale-vect (ycor-vect v) (edge2-frame frame))))))

;-------------
;  VECTORS
;-------------
;implements a vector from the origin to the point (x,y)
; constructor and accessors:
(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

; vector operations
; (vector, vector -> vector)
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))
; (vector, vector -> vector)
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))
; (vector, number -> vector)
(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
	     (* (ycor-vect v) s)))

;-------------
;   Frames
;-------------
; origin vector from 0,0 to point, bottom left corner
; edge1 from origin to right bottom corner of frame
; edlge2 from origin to top left corner of frame
; constructors and selectors
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;---------------
;   Segments
;---------------
;directed line sgements, with a start and end point
;represented by a vector from origin to start point, and a vector
;from origin to end point
;constructors and accessors:
(define (make-segment start-point end-point)
  (list start-point end-point))
(define start-segment car)
(define end-segment cadr)

;--------------
;   Painters
;--------------
; produces a painter (frame->#t): procedure takes a frame as argument
; and draws an image shifted and scaled to fit the frame

; segments->painter (list->procedure)
; draw-line: draws a line on the screen between two specified points
; segment-list: a list of line segments (pairs of points)
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
		(draw-line
		 ((frame-coord-map frame) (start-segment segment))
		 ((frame-coord-map frame) (end-segment segment))))
	      segment-list)))


;define unit square corners as vectors a, b, c, d
(define point-a (make-vect 0 0))
(define point-b (make-vect 1 0))
(define point-c (make-vect 1 1))
(define point-d (make-vect 0 1))
; gives midpoint of two vectors vector,vector -> vector
(define (midpoint v1 v2)
  (make-vect (/ (+ (xcor-vect v1) (xcor-vect v2)) 2.0)
	       (/ (+ (ycor-vect v1) (ycor-vect v2)) 2.0)))

;painter that draws the outline of the designated frame
(define (outline-frame frame)
  ((segments->painter (list (make-segment point-a point-b)
			    (make-segment point-b point-c)
			    (make-segment point-c point-d)
			    (make-segment point-d point-a))) frame))
				      
;painter that draws an "X" by connecting opposite corners of the frame
(define (x-frame frame)
  ((segments->painter (list (make-segment point-a point-c)
			    (make-segment point-b point-d))) frame))

;painter that draws a diamond shape, connecting midpoints of sides
;of frame
(define (diamond-frame frame)
  (let ((points (list (midpoint point-a point-b)
		      (midpoint point-b point-c)
		      (midpoint point-c point-d)
		      (midpoint point-d point-a)
		      (midpoint point-a point-b))))
    ((segments->painter (dot-to-dot points)) frame)))

; wave painter
(define (wave frame)
  (let ((point-groups
	 (list
	  ;added
	  (list (make-vect 0.4 0.85) (make-vect 0.45 0.8)
		(make-vect 0.55 0.8) (make-vect 0.6 0.85))
	  (list (make-vect 0.6 1)   (make-vect 0.65 0.85) (make-vect 0.6 0.7)
		(make-vect 0.7 0.7) (make-vect 1 0.4))
	  (list (make-vect 1 0.2)   (make-vect 0.6 0.5)   (make-vect 0.7 0))
	  (list (make-vect 0.6 0)   (make-vect 0.5 0.3)   (make-vect 0.4 0))
	  (list (make-vect 0.3 0)   (make-vect 0.4 0.55)  (make-vect 0.35 0.6)
		(make-vect 0.2 0.4) (make-vect 0 0.6))
	  (list (make-vect 0 0.8)   (make-vect 0.2 0.6)   (make-vect 0.3 0.7)
		(make-vect 0.4 0.7) (make-vect 0.35 0.85) (make-vect 0.4 1)))))
    (for-each (lambda (group)
		((segments->painter (dot-to-dot group)) frame))
	      point-groups)))

;list of points (vectors)--> list of segments, does not connect last
;dot to first
(define (dot-to-dot points)
  (cond ((null? points) nil)
	((null? (cdr points)) nil)
	(else (cons (make-segment (car points) (cadr points))
		    (dot-to-dot (cdr points))))))

