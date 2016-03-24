; Jacob Knispel
; Assignment 8a

; ---------------------------- Problem 1 ----------------------------
; (a)
(define (slist-map proc slist)
	(if (list? slist)
		(map (lambda (slist) (slist-map proc slist)) slist)
		(proc slist)
	))

; (b)
(define (slist-reverse slist)
	(if (list? slist)
		(reverse (map slist-reverse slist))
		slist
	))

; (c)
(define (slist-paren-count slist)
	(+ 2 (apply + (map slist-paren-count (filter list? slist)))))

; (d)
(define (slist-depth slist)
	(+ 1 (apply max 0 (map slist-depth (filter list? slist)))))

; (e)
(define (slist-symbols-at-depth slist d)
	(if (eq? d 1)
		(filter symbol? slist)
		(apply append (map (lambda (l) (if (list? l) (slist-symbols-at-depth l (- d 1)) '())) slist))
	))

; ---------------------------- Problem 2 ----------------------------
(define (group-by-two ls)
	(letrec ((helper (lambda (ls sublist)
		(cond 
			((null? ls) (if (null? sublist) '() (list (reverse sublist))))
			((eq? 2 (length sublist)) (cons (reverse sublist) (helper ls '())))
			(else (helper (cdr ls) (cons (car ls) sublist)))
		)
	)))
	(helper ls '())
))