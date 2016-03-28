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

; ---------------------------- Problem 4 ----------------------------
(define (subst-leftmost new old slist equality-pred?)
	; Helper returns tuple like (result, finished?)
	(letrec ((helper (lambda (slist)
		(cond 
			((null? slist) '(() #f))
			((list? slist) (let ((result (helper (car slist))))
									(if (cadr result)				; Short-circuit
										(list (cons (car result) (cdr slist)) #t)

										(let ((resultcdr (helper (cdr slist))))
											(list (cons (car result) (car resultcdr)) (cadr resultcdr))
										)
									)
								))
			((equality-pred? slist old) (list new #t))
			(else (list slist #f))
		)
	)))
	(car (helper slist))
))
