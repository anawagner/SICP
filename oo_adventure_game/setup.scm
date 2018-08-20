;;; SETUP.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;; PROJECT 4

;;;========================================================================
;;; You can extend this file to extend your world.
;;;========================================================================

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

(define (create-world)
  ; Create some places
  (let ((10-250 (create-place '10-250))
        (lobby-10 (create-place 'lobby-10))
        (grendels-den (create-place 'grendels-den))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (eecs-hq (create-place 'eecs-hq))
        (eecs-ug-office (create-place 'eecs-ug-office))
        (edgerton-hall (create-place 'edgerton-hall))
        (34-301 (create-place '34-301))
        (stata-center (create-place 'stata-center))
        (6001-lab (create-place '6001-lab))
        (building-13 (create-place 'building-13))
        (great-court (create-place 'great-court))
        (student-center (create-place 'student-center))
        (bexley (create-place 'bexley))
        (baker (create-place 'baker))
        (legal-seafood (create-place 'legal-seafood))
        (graduation-stage (create-place 'graduation-stage)))
    
    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways grendels-den 'up 'down lobby-10)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down 34-301)
    (can-go-both-ways 34-301 'up 'down eecs-hq)
    (can-go-both-ways 34-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'north 'south stata-center)
    (can-go-both-ways stata-center 'up 'down stata-center)
    (can-go-both-ways eecs-hq 'west 'east eecs-ug-office)
    (can-go-both-ways edgerton-hall 'north 'south legal-seafood)
    (can-go-both-ways eecs-hq 'up 'down 6001-lab)
    (can-go-both-ways legal-seafood 'east 'west great-court)
    (can-go-both-ways great-court 'up 'down graduation-stage)
    
    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-mobile-thing 'tons-of-code baker)
    (create-mobile-thing 'problem-set 10-250)
    (create-mobile-thing 'recitation-problem 10-250)
    (create-mobile-thing 'sicp stata-center)
    (create-mobile-thing 'engineering-book barker-library)
    (create-mobile-thing 'diploma graduation-stage)
    
    (list 10-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall 34-301 6001-lab
          building-13 great-court stata-center
          student-center bexley baker legal-seafood
          graduation-stage)))

; all spells exist in the chamber-of-stata.  When placing a spell
; in the outside world, the original spell from the chamber-of stata
; is cloned (using clone-spell; see objtypes.scm).
; There are no entrances, exits, or people in the chamber, preventing
;  the spells there from being stolen.
(define (instantiate-spells)
  (let ((chamber (create-place 'chamber-of-stata)))
    (create-spell
     'boil-spell
     chamber
     'PERSON
     "habooic katarnum"
     (lambda (caster target)
       (ask target 'EMIT (list (ask target 'NAME)
			       "grows boils on their nose"))))
    
    (create-spell
     'slug-spell
     chamber
     'PERSON
     "dagnabbit ekaterin"
     (lambda (caster target)
       (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
       (create-mobile-thing 'slug (ask target 'LOCATION))))

    (create-spell
     'wind-of-doom
     chamber
     'THING
     "mali spiritus"
     (lambda (caster target)
       (cond ((ask target 'IS-A 'PERSON)
	      (ask target 'EMIT
		   (list "A dark cold wind sweeps through"
			 (ask (ask target 'LOCATION) 'NAME)
			 "and knocks" (ask target 'NAME)
			 "off their feet"))
	      (ask target 'SUFFER (random-number 2) caster))
	     (else (ask target 'EMIT
			(list "A dark cold wind sweeps through"
			      (ask (ask target 'LOCATION) 'NAME)
			      "and" (ask target 'NAME)
			      "vanishes without a trace"))
		   (ask target 'DESTROY)))))
;; seal random thing from another person in the room (target does not matter)
    (create-spell
     'accipere
     chamber
     'THING
     "accipere"
     (lambda (caster target)
       (let ((loot (pick-random (ask caster 'PEEK-AROUND))))
	 (if loot
	     (ask caster 'take loot)
	     (ask caster 'EMIT '("but there is nothing to steal"))))))
;; disarm target, take their wand 
    (create-spell
     'expelliarmus
     chamber
     'PERSON
     "expelliarmus"
     (lambda (caster target)
       (map (lambda (wand) (ask caster 'take wand))
	    (ask target 'HAS-A 'WAND))))
;; disapear by leaving the room by a randomly chosen exit, target does
    ;; not matter
    (create-spell
     'disapperate
     chamber
     'THING
     "disapperate"
     (lambda (caster target)
       (let ((exit (pick-random (ask (ask caster 'LOCATION) 'EXITS))))
	 (ask caster 'GO-EXIT exit))))
     
    chamber))

(define (populate-spells rooms)
  (for-each (lambda (room)
	      (clone-spell (pick-random (ask chamber-of-stata 'THINGS)) room))
	    rooms))

(define (populate-special-items rooms)
  (let ((rings
	 (map (lambda (name)
		(create-ring-of-obfuscation name
					    (pick-random rooms)))
	      '(one-ring magic-ring)))
	(wands
	 (map (lambda (name)
		(create-wand name
			     (pick-random rooms)))
	      '(elder-wand unicorn-hair-wand
			   dragon-heartstring-wand wand-of-destiny))))
    (append rings wands)))

(define (populate-players rooms)
  (let* ((students (map (lambda (name)
			  (create-wit-student name
					      (pick-random rooms)
					      (random-number 3)
					      (random-number 3)))
			'(ben-bitdiddle alyssa-hacker
			  course-6-frosh lambda-man)))
;uncomment after writing professors
;	 (profs (map (lambda (name)
;		       (create-wit-professor name
;					     (pick-random rooms)
;					     (random-number 3)
;					     (random-number 3)))
;		     '(susan-hockfield eric-grimson)))
	 (monitors (map (lambda (name)
			  (create-hall-monitor name
					       (pick-random rooms)
					       (random-number 3)
					       (random-number 3)))
			'(dr-evil mr-bigglesworth)))
	 (trolls (map (lambda (name)
			(create-troll name
				      (pick-random rooms)
				      (random-number 3)
				      (random-number 3)))
		      '(grendel registrar))))

    (append students
;	    profs        ;uncomment after writing wit-professor
	    monitors trolls)))

(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define chamber-of-stata 'will-be-set-by-setup)

(define (setup name)
  (ask clock 'RESET)
  (ask clock 'ADD-CALLBACK
       (create-clock-callback 'tick-printer clock 'PRINT-TICK))
  (let ((rooms (create-world)))
    (set! chamber-of-stata (instantiate-spells))

    (populate-spells rooms)

    (populate-players rooms)

    (populate-special-items rooms)

    ;uncomment after writing chosen one
;    (create-chosen-one 'hairy-cdr (pick-random rooms)
;		       (random-number 3) (random-number 3))
    
    (set! me (create-avatar name (pick-random rooms)))
    (ask screen 'SET-ME me)
    (set! all-rooms rooms)
    'ready))

;; Some useful example expressions...

; (setup 'ben-bitdiddle)
; (run-clock 5)
; (ask screen 'DEITY-MODE #f)
; (ask screen 'DEITY-MODE #t)
; (ask me 'look-around)
; (ask me 'take (thing-named 'engineering-book))
; (ask me 'go 'up)
; (ask me 'go 'down)
; (ask me 'go 'north)
;
; (show me)
; (show screen)
; (show clock)
; (pp me)

(load "objsys.scm")
(load "objtypes.scm")
(setup 'ana)
;;(create-wit-student 'hermione (ask me 'location) 1 10)
;;(create-wit-student 'ron (ask me 'location) 1 10)
;;(create-wand 'dragon-heartstring-wand (thing-named 'hermione))
;;(create-wand 'unicorn-hair-wand (thing-named 'ron))
;;(map (lambda (spell) (clone-spell spell (ask me 'location)))
;;     (ask chamber-of-stata 'THINGS))
;;(create-wand 'mywand me)
;;(ask (thing-named 'hermione) 'take (thing-named 'boil-spell))
;;(ask (thing-named 'ron) 'take (thing-named 'slug-spell))

;; change accipere spell so that it shows a message when there are
;; no items to take
;;
;; change EAT-PEOPLE in troll let victim (pick-random...
