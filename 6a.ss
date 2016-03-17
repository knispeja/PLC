; Jacob Knispel
; Assignment 6a

; ---------------------------- Problem 1 ----------------------------
(define (curry2 func)
	(lambda (arg1)
		(lambda (arg2) (func arg1 arg2))
	))

; ---------------------------- Problem 2 ----------------------------
(define (curried-compose func)
	(lambda (arg)
		(lambda (x)
			(func (arg x))
		)))

; ---------------------------- Problem 3 ----------------------------
(define (compose . fxn-list)
	(letrec ((helper (lambda fxn-list
		(if (null? (cdr fxn-list))
			(lambda (arg) ((car fxn-list) arg))
			(lambda (arg) ((apply helper (cdr fxn-list)) ((car fxn-list) arg)))
		))))
	(apply helper (reverse fxn-list))
))

; ---------------------------- Problem 4 ----------------------------
(define (make-list-c num)
	(letrec ((helper (lambda (arg numLeft)
		(if (eq? numLeft 0)
			'()
			(cons arg (helper arg (- numLeft 1)))
		)
	)))
	(lambda (toDuplicate) (helper toDuplicate num))
))

; ---------------------------- Problem 5 ----------------------------
(define (let->application expr)
	(cons (list 'lambda (map car (cadr expr)) (caddr expr)) (map cadr (cadr expr)))
)

; ---------------------------- Problem 6 ----------------------------
(define (let*->let expr)
	(letrec ((helper (lambda (lets wrapMe)
		(if (null? lets)
			wrapMe
			(list 'let (list (car lets)) (helper (cdr lets) wrapMe))
		)
	)))
	(helper (cadr expr) (caddr expr))
))