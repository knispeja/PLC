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
	(and (matrix?-helper obj)
		 (apply eq? (map length obj))
	))

(define (matrix?-helper obj)
	(cond ((not (list? obj)) #f)
		  ((not (list? (car obj))) #f)
		  ((null? (car obj)) #f)
		  (else #t)
	))