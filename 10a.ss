; Jacob Knispel
; Assignment 10a

; ---------------------------- Problem 1 ----------------------------
(define (snlist-recur base listProc notListProc)
	(lambda (lst)
		(letrec ((recurse (lambda (lst)
			(cond
				((null? lst) base)
				((list? (car lst)) (listProc (recurse (car lst)) (recurse (cdr lst))))
				(else (notListProc (car lst) (recurse (cdr lst))))
			)
		)))
		(recurse lst)
	))
)