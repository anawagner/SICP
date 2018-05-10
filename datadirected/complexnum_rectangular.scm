;; complexnum_rectangular.scm
;; From Section 2.4 of SICP (May 9, 2018)
;; representation for complex numbers in rectangular form
;; by Ben Bitdiddle
;; example of data directed programming using dispatching on type
;; assuming implementation of put and get to insert and retreive from
;; table - to be implemented later

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))
  (define (angle z)
    (atan (image-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  ;; Note: operations will have multiple types, so a list is used
  ;;       constructors will only have one type.
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
