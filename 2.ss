; Jacob Knispel
; Assignment 2
; -------------------

; Problem 1
; a)
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
; b)
(define choose
  (lambda (n k)
    (/ (fact n)
       (* (fact k) (fact (- n k))))))

; Problem 2
(define range
  (lambda (m n)
  	(if (>= m n)
  		'()
  		(cons m (range (+ m 1) n)))))

; Problem 3
(define set?
  (lambda (lst)
  	(if (null? lst)
  		#t
  		(if (member (car lst) (cdr lst))
  			#f
  			(set? (cdr lst))))))

; Problem 4
(define sum-of-squares
	(lambda (lon)
		(if (null? lon)
			0
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))))

; Problem 5
(define make-vec-from-points
	(lambda (p1 p2)
		(if (null? p1)
			'()
			(cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2))))))

; Problem 6
(define dot-product
	(lambda (v1 v2)
		(if (null? v1)
			0
			(+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2))))))

; Problem 7
(define vec-length
	(lambda (v)
		(sqrt (sum-of-squares v))))

; Problem 8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

; Problem 9
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
			  (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
			  (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))

; Problem 10
(define parallel?
	(lambda (v1 v2)
		(= 0 (vec-length (cross-product v1 v2)))))

; Problem 11
(define collinear?
	(lambda (p1 p2 p3)
		(= 0 (vec-length (cross-product (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))))))