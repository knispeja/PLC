; Jacob Knispel
; Assignment 11a

; ---------------------------- Problem 1 ----------------------------

; (a)
(define-syntax my-let
	(syntax-rules ()
		(
			(_ expname ((x v) ...) e1 ...)
			(letrec ((expname (lambda (x ...) e1 ...)))
				(expname v ...))
		)
		(
			(_ ((x v) ...) e1 e2 ...)
			((lambda (x ...) e1 e2 ...)
				v ...)
		)
	)
)

; (b)
(define-syntax my-or
	(syntax-rules ()
		((_) #f)
		((_ e1) e1)
		(
			(_ e1 e2 ...)
			(let ((ev1 e1))
				(if ev1
					ev1
					(my-or e2 ...)
				)
			)
		)
	)
)

; (c)
(define-syntax +=
	(syntax-rules ()
		(
			(_ v num)
			(begin
				(set! v (+ v num))
				v
			)
		)
	)
)

; (d)
(define-syntax return-first
	(syntax-rules ()
		((_ e1) e1)
		(
			(_ e1 e2 ...)
			(let ((ev1 e1))
				(car (list ev1 e2 ...))
			)
		)
	)
)

; ---------------------------- Problem 2 ----------------------------
(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define (bintree-to-list tree)
	(cases bintree tree
		(leaf-node (field-name) (list 'leaf-node field-name))
		(interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right)))
	)
)

; ---------------------------- Problem 3 ----------------------------
(define (max-interior tree)
	; form = (sym, max, subtotal)
	(letrec ((helper (lambda (tree)
		(cases bintree tree
			(leaf-node (var) (list #f var var))
			(interior-node (key leftTree rightTree)
				(let* ((left (helper leftTree))
					  (right (helper rightTree))
					  (sbttl (+ (caddr left) (caddr right))))
					
					(append (max-symbol (list (list key sbttl) left right)) (list sbttl))
				)
			)
		)
	)))
	(car (helper tree))
))

; form = (sym, num)
(define (max-symbol symbs)
	(letrec ((helper (lambda (symbs max-sym max-num)
		(cond
			((null? symbs) (list max-sym max-num))
			((or (< (cadr (car symbs)) max-num)
				 (not (car (car symbs)))) 
					(helper (cdr symbs) max-sym max-num))
			(else (helper (cdr symbs) (car (car symbs)) (cadr (car symbs))))
		)
	)))
	(helper (cdr symbs) (car (car symbs)) (cadr (car symbs)))
))