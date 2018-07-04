;; 3.3.3 Representing Tables

(define (lookup key table)
  ;; lookup for one-dimensional table
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))
(define (insert! key value table)
  ;; insert! for one-dimensional table
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list *table*))

(define (lookup key-1 key-2 table)
  ;; lookup for two-dimensional table
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable (cons (cons key-2 value)
				       (cdr subtable)))))
	(set-cdr! table (cons (list key-1 (cons key-2 value))
			      (cdr table)))))
  'ok)
				      
;; Table represented procedurally as object that maintains a table
;; as part of its local state
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table (cons (list key-1
					      (cons key-2 value))
					(cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE " m))))
    dispatch))


;; get and put for data directed programming (section 2.4.3)
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))  ;;(get <key1> <key2>)->value
(define put (operation-table 'insert-proc!)) ;;(put <key1> <key2> <value>)

;; exercise 3.24
;; assoc uses equal? to check keys, use same-key? function instead
;; and make assoc an internal function
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table (cons (list key-1
					      (cons key-2 value))
					(cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE " m))))
    dispatch))


;; exercise 3.25
;; implement a table in which values are stored under an arbitrary
;; number of keys and different values may be stored under different
;; number of keys. lookup and insert! procedures take a list of keys
;; as input
(define (make-table1 same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-list)
      (define (iter k-list records)
	(if (null? k-list)
	    records
	    (let ((subtable (assoc (car k-list) records)))
	      (if subtable
		  (iter (cdr k-list) (cdr subtable))
		  false))))
      (iter key-list (cdr local-table)))
    (define (insert! key-list value)
      (define (make-record keys)
	(if (null? (cdr keys))
	    (cons (car keys) value)
	    (list (car keys) (make-record (cdr keys)))))
      (define (iter k-list table)
	(if (null? k-list)
	    (set-cdr! table value)
	    (let ((subtable (assoc (car k-list) (cdr table))))
	      (if subtable
		  (iter (cdr k-list) subtable)
		  (set-cdr! table (cons (make-record k-list)
					(cdr table)))))))
      (iter key-list local-table)
      'ok)
    (define (print)
      (do-print-table local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print) print)
	    (else (error "Unknown operation -- TABLE " m))))
    dispatch))

(define atable2 (make-table1 equal?))
(define put1 (atable2 'insert-proc!))
(define get1 (atable2 'lookup-proc))
(define tableprint (atable2 'print))
					;
(put1 '(letters a) 97)
(put1 '(letters b) 98)
(put1 '(math +) 43)
(put1 '(math -) 45)
(put1 '(math *) 42)
(put1 '(greek majiscule ^) 923)
(put1 '(greek miniscule <) 955)
(put1 '(min) 42)
(put1 '(max) 955)

