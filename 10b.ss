; Jacob Knispel
; Assignment 10b

; ---------------------------- Problem 1 ----------------------------
(define (free-vars e)
	(letrec ((helper (lambda (e binding)
		(cond
			((null? e) '())
			((list? e) (if (eq? (car e) 'lambda)
						   (helper (caddr e) (append (cadr e) binding))
						   (append-sets (helper (car e) binding) (helper (cdr e) binding))
						))
			((member e binding) '())
			(else (list e))
		))))
	(helper e '())
))

(define (bound-vars e)
	(letrec ((helper (lambda (e binding)
		(cond
			((null? e) '())
			((list? e) (if (eq? (car e) 'lambda)
						   (helper (caddr e) (append (cadr e) binding))
						   (append-sets (helper (car e) binding) (helper (cdr e) binding))
						))
			((member e binding) (list e))
			(else '())
		))))
	(helper e '())
))

(define (append-sets set1 set2)
	(cond
		((null? set1) set2)
		((member (car set1) set2) (append-sets (cdr set1) set2))
		(else (cons (car set1) (append-sets (cdr set1) set2)))
	)
)

; ---------------------------- Problem 2 ----------------------------
(define (free-vars e . bound-vars)
	(letrec ((helper (lambda (e binding)
		(cond
			((null? e) '())
			((list? e) (cond
							((eq? (car e) 'let) (append-sets 
															(free-vars-let (cadr e) binding)
															(helper (caddr e) (append (map car (cadr e)) binding))))
							((eq? (car e) 'let*) (append-sets
															(free-vars-let* (cadr e) binding)
															(helper (caddr e) (append (map car (cadr e)) binding))))
							((eq? (car e) 'lambda) (helper (caddr e) (append (cadr e) binding)))
							((eq? (car e) 'set!) (helper (caddr e) binding))
							(else (append-sets 
											(helper (car e) binding) 
											(helper (cdr e) binding)))
						))
			((member e binding) '())

			(else (list e))
		))))
	(if (null? bound-vars)
		(helper e '())
		(helper e (car bound-vars)))
))

(define (free-vars-let* lets bound-vars)
	(if (null? lets)
		'()
		(append-sets 
				(free-vars (cadr (car lets)) bound-vars) 
				(free-vars-let* (cdr lets) (cons (car (car lets)) bound-vars))
		)
	)
)

(define (free-vars-let lets bound-vars)
	(if (null? lets)
		'()
		(append-sets 
				(free-vars (cadr (car lets)) bound-vars) 
				(free-vars-let (cdr lets) bound-vars)
		)
	)
)

(define (occurs-free? sym e)
	(cond 
		((eq? sym 'let) #f)
		((eq? sym 'set!) #f)
		((eq? sym 'lambda) #f)
		((eq? sym 'let*) #f)
		((member sym (free-vars e)) #t)
		(else #f)
	)
)

(define (occurs-bound? sym e)
	(if (member sym (bound-vars e))
		#t
		#f
	)
)