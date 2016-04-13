; Jacob Knispel
; Assignment 10a

(define (free-vars e . binding)
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
	(if (null? binding)
		(helper e '())
		(helper e (car binding)))
))

(define (free-vars-let* lets binding)
	(if (null? lets)
		'()
		(append-sets 
				(free-vars (cadr (car lets)) binding) 
				(free-vars-let* (cdr lets) (cons (car (car lets)) binding))
		)
	)
)

(define (free-vars-let lets binding)
	(if (null? lets)
		'()
		(append-sets 
				(free-vars (cadr (car lets)) binding) 
				(free-vars-let (cdr lets) binding)
		)
	)
)

(define (occurs-free? sym e)
	(if (member sym (free-vars e))
		#t
		#f
	)
)

(define (bound-vars e . binding)
	(letrec ((helper (lambda (e binding)
		(cond
			((null? e) '())
			((list? e) (cond
							((eq? (car e) 'let) (append-sets 
															(bound-vars-let (cadr e) binding)
															(helper (caddr e) (append (map car (cadr e)) binding))))
							((eq? (car e) 'let*) (append-sets
															(bound-vars-let* (cadr e) binding)
															(helper (caddr e) (append (map car (cadr e)) binding))))
							((eq? (car e) 'lambda) (helper (caddr e) (append (cadr e) binding)))
							((eq? (car e) 'set!) (helper (caddr e) binding))
							(else (append-sets 
											(helper (car e) binding) 
											(helper (cdr e) binding)))
						))
			((member e binding) (list e))

			(else '())
		))))
	(if (null? binding)
		(helper e '())
		(helper e (car binding)))
))

(define (bound-vars-let* lets binding)
	(if (null? lets)
		'()
		(append-sets 
				(bound-vars (cadr (car lets)) binding) 
				(bound-vars-let* (cdr lets) (cons (car (car lets)) binding))
		)
	)
)

(define (bound-vars-let lets binding)
	(if (null? lets)
		'()
		(append-sets 
				(bound-vars (cadr (car lets)) binding) 
				(bound-vars-let (cdr lets) binding)
		)
	)
)

(define (occurs-bound? sym e)
	(if (member sym (bound-vars e))
		#t
		#f
	)
)

(define (append-sets set1 set2)
	(cond
		((null? set1) set2)
		((member (car set1) set2) (append-sets (cdr set1) set2))
		(else (cons (car set1) (append-sets (cdr set1) set2)))
	)
)