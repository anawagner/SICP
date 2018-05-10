;; Section 2.4.3 of SICP
;; Data-Directed Programming and Additivity
;; complex number generic operations that can use various
;; implementations
;;
;; Operations               types
;;            | polar           | rectangular
;; ----------------------------------------------
;; real-part  | real-part-polar | real-part-rectangular
;; imag-part  | imag-part-polar | imag-part-rectangular
;;   etc....
;;  a table is used to look up the actual procedure to execute
;;
;; entries on this table are added with
;;      (put <op> <type> <item>)
;; and accessed with
;;      (get <op> <type>)     returns item

(load "tagged_data.scm")
(load "complexnum_rectangular.scm")
(load "complexnum_polar.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
