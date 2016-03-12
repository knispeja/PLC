; Jacob Knispel
; Assignment 3

; ---------------------------- Problem 1 ----------------------------
; (imports from 2.ss)
(define (sum-of-squares lon)
		(if (null? lon)
			0
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))))
(define (make-vec-from-points p1 p2)
	(if (null? p1)
		'()
		(cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2)))))
(define (vec-length v)
	(sqrt (sum-of-squares v)))
(define (distance p1 p2)
	(vec-length (make-vec-from-points p1 p2)))

; Solving the problem
(define (nearest-point p list-of-points)
	(cond ((null? (cdr list-of-points))
		   (car list-of-points))
		  ((>= (distance p (nearest-point p (cdr list-of-points))) (distance p (car list-of-points)))
		   (car list-of-points))
		  (else (nearest-point p (cdr list-of-points)))
	))

; ---------------------------- Problem 2 ----------------------------
(define (union set1 set2) 
	(if (null? set2)
  		set1
  		(if (member (car set2) set1)
  			(union set1 (cdr set2))
  			(union (cons (car set2) set1) (cdr set2))
  		)))

; ---------------------------- Problem 3 ----------------------------
(define (intersection set1 set2)
	(if (< (length set1) (length set2))
		(intersection-helper set1 set2 '())
		(intersection-helper set2 set1 '())
	))

(define (intersection-helper set1 set2 result)
	(if (null? set1)
  		result
  		(if (member (car set1) set2)
  			(intersection-helper (cdr set1) set2 (cons (car set1) result))
  			(intersection-helper (cdr set1) set2 result)
  		)))

; ---------------------------- Problem 4 ----------------------------
(define (subset? possSub set)
	(if (null? possSub)
		#t
		(if (member (car possSub) set)
			(subset? (cdr possSub) set)
			#f)))

; ---------------------------- Problem 5 ----------------------------
(define (set? lst)
  	(cond ((not (list? lst)) #f)
  		  ((null? lst) #t)
  		  ((member (car lst) (cdr lst)) #f)
  		  (else (set? (cdr lst)))
    ))

(define (relation? relation)
	(if (set? relation)
		(relation?-helper relation)
		#f
	))

(define (relation?-helper relation)
	(cond ((null? relation) #t)
		  ((list? (car relation)) (and (eq? 2 (length (car relation)))
		  							   (relation?-helper (cdr relation))))
		  (else #f)
    ))

; ---------------------------- Problem 6 ----------------------------
(define (domain r)
	(if (null? r)
		'()
		(addToSet (car (car r)) (domain (cdr r)))
	))

(define (addToSet value set)
	(if (containedInSet? value set)
		set
		(cons value set)))

(define (containedInSet? value set)
	(cond ((null? set) #f)
		  ((eq? value (car set)) #t)
		  (else (containedInSet? value (cdr set)))
	))

; ---------------------------- Problem 7 ----------------------------
(define (reflexive? r)
	(sets-equal? (domain (removeNonReflexivePairs r)) (union (domain r) (range r))))

(define (range r)
	(if (null? r)
		'()
		(addToSet (cadr (car r)) (domain (cdr r)))
	))

(define (removeNonReflexivePairs r)
	(cond ((null? r) '())
		  ((eq? (car (car r)) (cadr (car r))) (cons (car r) (removeNonReflexivePairs (cdr r))))
		  (else (removeNonReflexivePairs (cdr r)))
	))

(define (sets-equal? s1 s2)
	(and (subset? s1 s2) (subset? s2 s1)))

; ---------------------------- Problem 7 ----------------------------
(define (hailstone-step-count n)
	(cond ((eq? n 1) 0)
		  ((eq? (modulo n 2) 0) (+ (hailstone-step-count (/ n 2)) 1))
		  (else (+ (hailstone-step-count (+ (* 3 n) 1)) 1))
	))