; Jacob Knispel
; Assignment 11b

; Answer to question in problem 2:
; 

(define (apply-continuation k v)
	(k v)
)

; ---------------------------- Problem 1 ----------------------------

; (a)
(define (member?-cps sym ls k)
	(cond
		((null? ls) (apply-continuation k #f))
		((equal? (car ls) sym) (apply-continuation k #t))
		(else (member?-cps sym (cdr ls) k))
	)
)

; (b)
(define (set?-cps ls k)
	(cond
		((null? ls) (apply-continuation k #t))
		((not (pair? ls)) (apply-continuation k #f))
		(else (set?-cps
					(cdr ls)
					(lambda (isStillSet?)
						(member?-cps (car ls) (cdr ls)
							(lambda (isIn?)
								(apply-continuation k (and isStillSet? (not isIn?)))
							)
						)
					)
				))
	)
)

; (d) NOTE: This is above c for load order...
(define (make-cps non-cps)
	(lambda (arg k)
		(apply-continuation k (non-cps arg))
	)
)

; (c)
(define 1st-cps (make-cps car))

(define (set-of-cps ls k)
	(if (null? ls) 
		(apply-continuation k '())
		(set-of-cps
			(cdr ls)
			(lambda (set)
				(member?-cps (car ls) (cdr ls)
					(lambda (isIn?)
						(if isIn?
							(apply-continuation k set)
							(apply-continuation k (cons (car ls) set))
						)
					)
				)
			)
		)
	)
)

(define (map-cps proc-cps ls k)
	(if (null? ls) 
		(apply-continuation k '())
		(map-cps
			proc-cps
			(cdr ls)
			(lambda (results)
				(proc-cps 
					(car ls) 
					(lambda (result) 
						(apply-continuation k (cons result results))
					)
				)
			)
		)
	)
)

(define (domain-cps ls k)
	(map-cps
		1st-cps
		ls
		(lambda (x)
			(set-of-cps x k)
		)
	)
)

; (e)
(define (andmap-cps pred-cps ls k)
	(if (null? ls) 
		(apply-continuation k #t)
		(pred-cps
			(car ls)
			(lambda (result)
				(if result
					(andmap-cps pred-cps
						(cdr ls) 
						(lambda (x) 
							(apply-continuation k x)
						)
					)
					(apply-continuation k #f)
				)
			)
		)
	)
)

; (f)
(define (cps-snlist-recur base-val item-proc-cps list-proc-cps)
	(letrec ((helper (lambda (ls k)
		(if (null? ls)
			(apply-continuation k base-val)
			(let ((c (car ls)))
				(helper 
					(cdr ls) 
					(lambda (resultCDR)
						(if (or (pair? c) (null? c))
							(helper
								c
								(lambda (resultC)
									(list-proc-cps resultC resultCDR (lambda (x) (apply-continuation k x)))
								))
							(item-proc-cps c resultCDR (lambda (x) (apply-continuation k x)))
						)
					)
				)

				;(if (or (pair? c) (null? c))
				;	(helper
				;		(cdr ls)
				;		(lambda (resultCDR)
				;			(helper
				;				c
				;				(lambda (resultC)
				;					(list-proc-cps
				;						resultC
				;						resultCDR
				;						(lambda (x) (apply-continuation k x))
				;					)
				;				)
				;			)
				;		))
				;	(helper
				;		(cdr ls)
				;		(lambda (resultCDR)
				;			(item-proc-cps
				;				c
				;				resultCDR
				;				(lambda (x) (apply-continuation k x))
				;			)
				;		)
				;	)
				;)
			)
		)
	)))
	helper
))

(define (+-cps a b k)
		(apply-continuation k (+ a b)))

(define sn-list-sum-cps
	(cps-snlist-recur 0 +-cps +-cps))


; ---------------------------- Problem 2 ----------------------------
(define (memoize f hash equiv?)