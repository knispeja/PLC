; Jacob Knispel
; Assignment 5

; ---------------------------- Problem 1 ----------------------------

; (From 1.ss)
(define (interval-contains? interval number)
		(and (>= number (car interval))
			 (<= number (cadr interval))))
(define (interval-intersects? i1 i2)
		(or (interval-contains? i1 (car i2))
			(interval-contains? i2 (car i1))))
(define (interval-union i1 i2)
		(if (interval-intersects? i1 i2)
			(list
				(list 
					(if (< (car i1) (car i2))
					    (car i1)
					   	(car i2))
				    (if (> (cadr i1) (cadr i2))
				  	    (cadr i1)
				  	    (cadr i2))))
			(list i1 i2)))

(define (minimize-interval-list ls)
	(if (null? ls)
		(append (minimize-list-via-range ls (car ls)) (product (cdr set1) set2))
		'()
	)) 

; ---------------------------- Problem 2 ----------------------------
(define (exists? pred ls)
	(cond
		((null? ls) #f)
		((pred (car ls)) #t)
		(else (exists? pred (cdr ls)))
	))

; ---------------------------- Problem 3 ----------------------------
(define (list-index pred ls . i)
	(cond
		((null? i) (list-index pred ls 0))
		((null? ls) #f)
		((pred (car ls)) (car i))
		(else (list-index pred (cdr ls) (+ (car i) 1)))
	))

; ---------------------------- Problem 4 ----------------------------
(define (fact n)
    (if (zero? n)
        1
        (* n (fact (- n 1)))))

(define (choose n k)
    (/ (fact n)
       (* (fact k) (fact (- n k)))))

(define (pascal-row n . k)
	(cond
		((null? k) (pascal-row n n))
		((< (car k) 0) '())
		(else (cons (choose n (car k)) (pascal-row n (- (car k) 1))))
	))

(define (pascal-triangle n)
	(if (< n 0)
		'()
		(cons (pascal-row n) (pascal-triangle (- n 1)))
	))

; ---------------------------- Problem 5 ----------------------------
(define (product set1 set2)
	(if (or (null? set1) (null? set2))
		'()
		(append (product-single set2 (car set1)) (product (cdr set1) set2))
	)) 

(define (product-single set x)
	(if (null? set)
		'()
		(cons (list x (car set)) (product-single (cdr set) x))
	))