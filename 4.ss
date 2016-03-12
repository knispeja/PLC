; Jacob Knispel
; Assignment 4

; ---------------------------- Problem 1 ----------------------------
(define (multi-set? lst)
	(if (multi-set lst) #t #f))

(define (multi-set lst)
	(cond ((not (list? lst)) #f)
		  ((null? lst) '())
		  ((not (list? (car lst))) #f)
		  ((not (eq? (length (car lst)) 2)) #f)
		  ((not (integer? (cadr (car lst)))) #f)
		  ((<= (cadr (car lst)) 0) #f)
		  (else (let ((firsts (multi-set (cdr lst))))
				  (cond ((not (list? firsts)) firsts)
					    ((member (car (car lst)) firsts) #f)
					    (else (cons (car (car lst)) firsts))
				  )))
	))

; ---------------------------- Problem 2 ----------------------------
(define (ms-size ms)
	(apply + (map cadr ms)))

; ---------------------------- Problem 3 ----------------------------
(define (matrix-ref m row col)
	(list-ref (list-ref m row) col))

; ---------------------------- Problem 4 ----------------------------
(define (matrix? obj)
	(cond ((not (list? obj)) #f)
		  ((not (andmap list? obj)) #f) 
		  ((ormap null? obj) #f)
		  ((not (sublist-elements-integers? obj)) #f)
		  ((not (sublist-lengths-equal? obj -1)) #f)
		  (else #t)
	))

(define (sublist-elements-integers? ls)
	(if (null? ls)
		#t
		(and (andmap integer? (car ls)) (sublist-elements-integers? (cdr ls)))
	))

(define (sublist-lengths-equal? ls len)
	(cond ((null? ls) #t)
		  ((< len 0) (sublist-lengths-equal? (cdr ls) (length (car ls))))
		  (else (and (eq? (length (car ls)) len) (sublist-lengths-equal? (cdr ls) len)))
	))

; ---------------------------- Problem 5 ----------------------------
(define (matrix-transpose m)
	(if (null? (car m))
		'()
		(cons (apply list (map car m)) (matrix-transpose (apply list (map cdr m))))
	))

; ---------------------------- Problem 6 ----------------------------
(define (last ls)
	(if (null? (cdr ls))
		(car ls)
		(last (cdr ls))
	))

; ---------------------------- Problem 7 ----------------------------
(define (all-but-last ls)
	(if (null? (cdr ls))
		'()
		(cons (car ls) (all-but-last (cdr ls)))
	))