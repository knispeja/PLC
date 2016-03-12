; Jacob Knispel
; Assignment 1
; -------------------

; Problem 1
(define Fahrenheit->Celsius
	(lambda (temperature)
		(* (/ 5 9) (- temperature 32))))

; Problem 2
(define interval-contains?
	(lambda (interval number)
		(and (>= number (car interval))
			 (<= number (cadr interval)))))

; Problem 3
(define interval-intersects?
	(lambda (i1 i2)
		(or (interval-contains? i1 (car i2))
			(interval-contains? i2 (car i1)))))

; Problem 4
(define interval-union
	(lambda (i1 i2)
		(if (interval-intersects? i1 i2)
			(list
				(list 
					(if (< (car i1) (car i2))
					    (car i1)
					   	(car i2))
				    (if (> (cadr i1) (cadr i2))
				  	    (cadr i1)
				  	    (cadr i2))))
			(list i1 i2))))

; Problem 5
(define divisible-by-7?
	(lambda (num)
		(eq? (modulo num 7) 0)))

; Problem 6
(define ends-with-7?
	(lambda (num)
		(eq? (modulo num 10) 7)))

; Problem 7
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)