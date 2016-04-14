; Jacob Knispel
; Assignment 10b

; ---------------------------- Problem 3 ----------------------------
(define (lexical-address e)
	(letrec ((helper (lambda (e binding d)
		(cond
			((null? e) '())
			((list? e) (case (car e)
							((if) (cons 'if (helper (cdr e) binding d)))
							((lambda) (list 'lambda (cadr e) (helper (caddr e) (lex-dict-set-many binding (cadr e) (+ 1 d)) (+ 1 d))))
							((set!) (list 'set! (cadr e) (helper (caddr e) binding d)))
							((let) (letrec ((lethelper (lambda (letexp)
										(if (null? letexp)
											'()
											(cons (cons (car (car letexp)) (list (helper (cadr (car letexp)) binding d))) (lethelper (cdr letexp)))
										))))
										(list 'let 
											 	(lethelper (cadr e)) 
											 	(helper (caddr e) (lex-dict-set-many binding (map car (cadr e)) (+ 1 d)) (+ 1 d))
										)))
							(else (cons
										(helper (car e) binding d)
										(helper (cdr e) binding d)))
						))
			((lex-dict-get binding e) (list ': (- d (car (lex-dict-get binding e))) (cadr (lex-dict-get binding e))))
			(else (list ': 'free e))
		))))
	(helper e '() -1)
))

; lex-dict holds entries like (var, (depth, pos))
(define (lex-dict-get dict elem)
	(cond
		((null? dict) #f)
		((eq? (car (car dict)) elem) (cadr (car dict)))
		(else (lex-dict-get (cdr dict) elem))
	))

(define (lex-dict-set-many dict elems depth)
	(letrec ((helper (lambda (dict elems pos)
		(if (null? elems)
			dict
			(helper (lex-dict-set dict (car elems) depth pos) (cdr elems) (+ 1 pos))
		)
	)))
	(helper dict elems 0)
))

(define (lex-dict-set dict elem depth pos)
	(cond
		((null? dict) (list (list elem (list depth pos))))
		((eq? (car (car dict)) elem) (cons (list elem (list depth pos)) (cdr dict)))
		(else (cons (car dict) (lex-dict-set (cdr dict) elem depth pos)))
	))

; ---------------------------- Problem 4 ----------------------------
(define (un-lexical-address e)
	(letrec ((helper (lambda (e binding d)
		(cond
			((null? e) '())
			((list? e) (case (car e)
							((:) (if (eq? (cadr e) 'free)
									(caddr e)
									(lex-dict-get-elem binding (- d (cadr e)) (caddr e))))
							((if) (cons 'if (helper (cdr e) binding d)))
							((lambda) (list 'lambda (cadr e) (helper (caddr e) (lex-dict-set-many binding (cadr e) (+ 1 d)) (+ 1 d))))
							((set!) (list 'set! (cadr e) (helper (caddr e) binding d)))
							((let) (letrec ((lethelper (lambda (letexp)
										(if (null? letexp)
											'()
											(cons (cons (car (car letexp)) (list (helper (cadr (car letexp)) binding d))) (lethelper (cdr letexp)))
										))))
										(list 'let 
											 	(lethelper (cadr e)) 
											 	(helper (caddr e) (lex-dict-set-many binding (map car (cadr e)) (+ 1 d)) (+ 1 d))
										)))
							(else (cons
										(helper (car e) binding d)
										(helper (cdr e) binding d)))
						))
			(else 'PROBLEM)
		))))
	(helper e '() -1)
))

(define (lex-dict-get-elem dict depth pos)
	(cond
		((null? dict) #f)
		((and (eq? (car (cadr (car dict))) depth) (eq? (cadr (cadr (car dict))) pos)) (car (car dict)))
		(else (lex-dict-get-elem (cdr dict) depth pos))
	))