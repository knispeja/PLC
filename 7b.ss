; Jacob Knispel
; Assignment 7b

; ---------------------------- Problem 6 ----------------------------
(define (map-by-position fxn-list arg-list)
	(map (lambda (fxn arg) (fxn arg)) fxn-list arg-list))

; ---------------------------- Problem 7 ----------------------------
(define (bt-leaf-sum T)
	(if (list? T)
		(+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T)))
		T
	))

(define (bt-inorder-list T)
	(if (list? T)
		(append (bt-inorder-list (cadr T)) (cons (car T) (bt-inorder-list (caddr T))))
		'()
	))

(define (bt-max T)
	(if (list? T)
		(max (bt-max (cadr T)) (bt-max (caddr T)))
		T))

(define (bt-max-interior T)
	(letrec ((bt-subtotal (lambda (T)
			(if (not (list? T))
				T 
				(let ((left (bt-subtotal (cadr T))) ; bthelper is called only once on each node (one tree traversal --> O(N))
					(right (bt-subtotal (caddr T))))

					(let ((subtotal (add-pairs-and-nums left right)))
						(max-pairs-and-nums (list (list (car T) subtotal subtotal) (update-subtotal left subtotal) (update-subtotal right subtotal)))
					)
				)
			)
		)))
	(car (bt-subtotal T))
))

; pn is something that could be either a btpair -- form: (symbol, int) -- or an integer, we don't know because bthelper can return both
; This function updates the subtotal value in the max subtotal objects being passed up the recursive calls
(define (update-subtotal pn newsub)
	(if (list? pn)
		(list (car pn) (cadr pn) newsub)
		pn))

; Adds together two objects known to be either a btpair or an int
(define (add-pairs-and-nums pn1 pn2)
	(+ (if (list? pn1)
				(caddr pn1)
				pn1) 
		(if (list? pn2)
				(caddr pn2)
				pn2)
	))

; The pairs list (full of btpairs) cannot be larger than 3, so the runtime of the overall is about O(3N)
(define (max-pairs-and-nums pairs)
	(letrec ((helper (lambda (pairs numToFind)
			(if (eq? (cadr (car pairs)) numToFind)
				(car pairs)
				(helper (cdr pairs) numToFind)
			))))
	(let ((filteredPairs (filter list? pairs)))
		(helper filteredPairs (apply max (map cadr filteredPairs)))
	)
))