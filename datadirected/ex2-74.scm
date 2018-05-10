;; exercise 2.74 - company with independent divisions, all have
;; implemented records with scheme data structures, though they vary
;; from division to division.

;; each divison's personnel records consist of a single file 
;; containing a set of records keyed on employee's names, structure
;; varies from division to divison. each employee record is a set of
;; records keyed under identifiers such as address, salary, etc

;; a)  implement get-record
;;
;;  operation   |  types (divisons) 
;;              |  div1    | div2 .... etc
;; ------------------------------------------------
;; get-record   | div1-get | div2-get.... .etc
;;
;;  each division should add their get-record procedure to the table
;; using
;; (put 'get-record <division name> <division get record procedure>)

(define (get-record key) (apply-generic 'get-record key))

(define (get-salary key) (apply-generic 'get-salary key))

