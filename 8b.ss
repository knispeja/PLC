; Jacob Knispel
; Assignment 8b

; ---------------------------- Problem 3 ----------------------------
(define (group-by-n ls n)
	(letrec ((helper (lambda (ls sublist)
		(cond 
			((null? ls) (if (null? sublist) '() (list (reverse sublist))))
			((eq? n (length sublist)) (cons (reverse sublist) (helper ls '())))
			(else (helper (cdr ls) (cons (car ls) sublist)))
		)
	)))
	(helper ls '())
))