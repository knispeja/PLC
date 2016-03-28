; Jacob Knispel
; Assignment 9

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

; (a)
(define (sn-list-sum snlst)
	((snlist-recur 0 + +) snlst))

; (b)
(define (sn-list-map proc snlst)
	((snlist-recur '() 
					cons 
					(lambda (x lst) 
							(cons (proc x) lst))
	) snlst))

; (c)
(define (sn-list-paren-count snlst)
	((snlist-recur 
				2
				+
				(lambda (x y) 
						y)
	) snlst))

; (d)
(define (sn-list-reverse snlst)
	((snlist-recur 
				'()
				(lambda (ls1 ls2)
					(append ls2 (list ls1)))
				(lambda (x ls2)
					(append ls2 (list x)))
	) snlst))

; (e)
(define (sn-list-occur s snlst)
	((snlist-recur 
				0
				+
				(lambda (x y)
					(+ y (if (eq? s x) 1 0)))
	) snlst))

; (f)
(define (sn-list-depth snlst)
	((snlist-recur 
				1
				(lambda (x y) (max (+ 1 x) y))
				(lambda (x y) y)
	) snlst))

; ---------------------------- Problem 2 ----------------------------
(define (bt-recur joinProc elemProc)
	(lambda (bt)
		(letrec ((recurse (lambda (bt)
			(if (list? bt) 
				(joinProc (car bt) (recurse (cadr bt)) (recurse (caddr bt)))
				(elemProc bt)
			)
		)))
		(recurse bt)
	))
)

(define (bt-sum t)
	((bt-recur 
		(lambda (e bt1 bt2) (+ bt1 bt2))
		(lambda (e) e)
	) t))

(define (bt-inorder t)
	((bt-recur 
		(lambda (e bt1 bt2) (append (append bt1 (list e)) bt2))
		(lambda (e) '())
	) t))

; ---------------------------- Problem 3 ----------------------------
(define (make-c...r str)
	(apply compose
		(map (lambda (letter)
					(if (eq? letter #\a)
						car
						cdr))
					(string->list str))))

(define compose
	 (case-lambda
	 (() (lambda (x) x))
	 ((first . rest)
	 (let ((composed-rest (apply compose rest)))
	 (lambda (x) (first (composed-rest x)))))))

; ---------------------------- Problem 4 ----------------------------
(define (make-slist-leaf-iterator lst)
	(define stk (make-stack))
	(stk 'push lst)

	(lambda ()
		(leaf-iter stk)
	)
)

(define (leaf-iter stk)
	(if (stk 'empty?)
		#f
		(let ((popped (stk 'pop)))
			(cond
				((null? popped) (leaf-iter stk))
				((list? popped)  (stk 'push (cdr popped))
								 (stk 'push (car popped))
								 (leaf-iter stk))
				(else popped)
			)
		)
	)
)

(define make-stack
(lambda ()
 (let ([stk '()])
 (lambda (msg . args )
 (case msg ; Scheme's case is a similar to switch in some other languages.
	 [(empty?) (null? stk)]
	 [(push) (set! stk (cons (car args) stk))]
	 [(pop) (let ([top (car stk)])
	 	(set! stk (cdr stk))
		top)]
	 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))