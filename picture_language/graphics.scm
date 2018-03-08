;must install xQuartz on mac os x to get 'x' graphic device

(load "picture_language.scm")
(load "transformations.scm")

(define mydevice
  (if (graphics-type-available? 'x)
      (make-graphics-device 'x)
      (else (display "not available"))))

;(display (graphics-coordinate-limits mydevice))

;vector,vector->unspecified: draws a line from first vector (point)
;to the second one
(define (draw-line start end)
  (graphics-draw-line mydevice
		      (xcor-vect start) (ycor-vect start)
		      (xcor-vect end)   (ycor-vect end)))

(define aframe (make-frame (make-vect -0.5 -0.5)
			   (make-vect 1 0)
			   (make-vect 0 1)))
;(graphics-clear mydevice)
;(outline-frame aframe)
;(x-frame aframe)
;(diamond-frame aframe)
;((flip-vert wave) aframe)
;((shrink-to-upper-right wave) aframe)
;((rotate90 wave) aframe)
;((squash-inwards wave) aframe)
;((beside wave (flip-vert wave)) aframe)
;((flip-horiz wave) aframe)
;((rotate180 wave) aframe)
;((rotate270 wave) aframe)
;((below1 diamond-frame x-frame) aframe)
;((flipped-pairs wave) aframe)
;((square-limit wave 4) aframe)
;((corner-split wave 4) aframe)
;((corner-split2 wave 4) aframe)
;((wallpaper (flipped-pairs wave) 2) aframe)
;(graphics-clear mydevice)
;(graphics-close mydevice)

