; Jacob Knispel
; Assignment 5b

; ---------------------------- Problem 6 ----------------------------
(define (max-edges n) (/ (* n (- n 1)) 2))

; ---------------------------- Problem 7 ----------------------------
(define (complete? G)
	(letrec ((helper (lambda (firsts rest num) 
				(cond 
					((null? firsts) #t)
					((eq? (count (car firsts) rest) num) (helper (cdr firsts) rest num))
					(else #f)
				)
			)))
	(helper (map car G) (apply append (apply append (map cdr G))) (- (length G) 1))
))

(define (count value ls)
	(cond
		((null? ls) 0)
		((equal? (car ls) value) (+ 1 (count value (cdr ls))))
		(else (count value (cdr ls)))
	)
)

; ---------------------------- Problem 8 ----------------------------
(define (complete ls)
	(letrec ((helper (lambda (remaining alpha) 
				(if (null? remaining)
					'()
					(cons (list (car remaining) (remove-first (car remaining) alpha)) (helper (cdr remaining) alpha))
				)
			)))
	(helper ls ls)
))

; ---------------------------- Problem 9 ----------------------------
(define (replace old new ls)
	(cond
		((null? ls) '())
		((equal? old (car ls)) (cons new (replace old new (cdr ls))))
		(else (cons (car ls) (replace old new (cdr ls))))
	))

; ---------------------------- Problem 10 ----------------------------
(define (remove-first element ls)
	(cond 
		((null? ls) '())
		((equal? element (car ls)) (cdr ls))
		(else (cons (car ls) (remove-first element (cdr ls))))
	))

; ---------------------------- Problem 11 ----------------------------
(define (remove-last element ls)
	(reverse (remove-first element (reverse ls))))