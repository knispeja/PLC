; Jacob Knispel
; Assignment 6b

; ---------------------------- Problem 7 ----------------------------
(define (filter-in pred? lst)
	(cond
		((null? lst) '())
		((pred? (car lst)) (cons (car lst) (filter-in pred? (cdr lst))))
		(else (filter-in pred? (cdr lst)))
	))

; ---------------------------- Problem 8 ----------------------------
(define (filter-out pred? lst)
	(cond
		((null? lst) '())
		((pred? (car lst)) (filter-out pred? (cdr lst)))
		(else (cons (car lst) (filter-out pred? (cdr lst))))
	))

; ---------------------------- Problem 9 ----------------------------
(define (sort-list-of-symbols los)
	(map string->symbol (sort string<? (map symbol->string los))))

; ---------------------------- Problem 10 ----------------------------
(define (invert ls)
	(map reverse ls))

; ---------------------------- Problem 11 ----------------------------
(define (vector-index pred vec)
	(letrec ((helper (lambda (i)
		(cond
			((eq? i (vector-length vec)) #f)
			((pred (vector-ref vec i)) i)
			(else (helper (+ i 1)))
		))))
	(helper 0)
))