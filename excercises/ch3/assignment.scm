;; 3.1.2 The benefits of Introducing Assignment
;; see footnote 6 for discussion of randomness

(define rand
  ;; given in text
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define random-init 1)
(define (rand-update x)
  ;; generates random number 0 - 99
  (random 100))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; exercise 3.5 Monte Carlo integration
;; radius = 3  Center = (5,7) Predicate: (x - 5)^2 + (y - 7)^2 <= 3^2
;; test area: rectangle with opposite corners (2, 4) & (8, 10)
;; fraction of points that pass * area of rectangle = integral
;; procedure estimate-integral ( P - predicate, upper & lower bounds:
;; x1, x2, y1, y2, and number of trials)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (in-circle? Cx Cy r)
  ;; creates a predicate that tests if x y values are inside a circle
  ;; with center (Cx, Cy) and radius r
  (lambda (x y)
    (not (> (+ (square (- x Cx)) (square (- y Cy))) (square r)))))
(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((integral-test
	 (lambda ()
	   (p (random-in-range x1 x2)
	      (random-in-range y1 y2))))
	(rect-area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials integral-test) rect-area)))

;; takes more than 2 mins to execute with this number of trials,
;; but gives a better estimate
;;(estimate-integral (in-circle? 5 7 3) 2.0 8.0 4.0 10.0 10000000.0)
;;(estimate-integral (in-circle? 1 1 1) 0.0 2.0 0.0 2.0 10000000.0)


;; exercise 3.6
;; (rand 'generate) produces a new random number
;; ((rand 'reset) <new-value>) resets internal state variable
;; this should allow user to generate pseudo-random repeatable
;; sequences

(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? message 'reset)
	     (lambda (new-value)
	       (set! x new-value)))
	    (else (error "Unknown message - " message))))
    dispatch))
     
(define random-init 1)
(define (rand-update x)
  (let ((m 127)
	(a 27)
	(b 26))
    (modulo (+ (* a x) b) m)))

(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
((rand 'reset) 3)
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
((rand 'reset) 5)
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
((rand 'reset) 3)
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
((rand 'reset) 5)
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))
((rand 'reset) 3)
(list (rand 'generate) (rand 'generate)
      (rand 'generate) (rand 'generate))

;; 3.1.3 the pitfalls of imperatie programming

;; exercise 3.7
;; create a Make-joint procedure to create joint access to 
;; an existing pw protected account. the new access account will have
;; its own password
(define (make-joint account-name account-pw joint-account-pw)
  (lambda (pw m)
    (if (eq? pw joint-account-pw)
	(account-name account-pw m)
	(lambda (amt) (display "Incorrect Password")))))

(load "local_state_variables.scm")

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 11)
((peter-acc 'open-sesame 'deposit) 10)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 30)
((peter-acc 'open-sesame 'deposit) 10)
((paul-acc 'rosebud 'deposit) 20)
((peter-acc 'rosebud 'withdraw) 85)
((paul-acc 'open-sesame 'withdraw) 50)
((peter-acc 'open-sesame 'withdraw) 50)


;; exercise 3.8
;; define a procedure f such that (+ (f 0) (f 1))
;; returns 0 if evaluated left to right
;; returns 1 if evaluated right to left
(define f
  (let ((y 1))
    (lambda (x)
      (set! y (* x y))
      y)))
;; = 0
;;(f 0)
;;(f 1)

;; = 1
(f 1)
(f 0)
