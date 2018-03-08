#lang sicp
(#%require sicp-pict)

; function, number --> function
; repeats function n times (returns a function that applies the original
; function the number of times given)
; the returned function takes one argument 
(define (repeated f n)
  (if (not (> n 1))
      f
      (lambda (x) (f ((repeated f (- n 1)) x)))))

;function --> function(painter, number)
(define (push comb)
  (lambda (pict n)
    ((repeated
      (lambda (p) (comb pict p))
      n)
     pict)))

; example usage: (paint (right-push eisntein 3))
(define right-push (push beside))
(define up-push (push below))

; creates pattern from sicp 2.2.4 fig 2.13
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

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)))
    (combine4 (corner-split painter n))))