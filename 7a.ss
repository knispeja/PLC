; Jacob Knispel
; Assignment 7a

; ---------------------------- Problem 1 ----------------------------
(define (vector-append-list v lst)
	(letrec ((helper (lambda (newv lst i)
		(cond 
			((> (vector-length v) i) (vector-set! newv i (vector-ref v i)) (helper newv lst (+ 1 i)))
			((null? lst) newv)
			(else (vector-set! newv i (car lst)) (helper newv (cdr lst) (+ 1 i)))
		))))
	(helper (make-vector (+ (length lst) (vector-length v))) lst 0)
))

; ---------------------------- Problem 2 ----------------------------
(define (qsort pred ls)
	(letrec ((partitionList (lambda (pivot ls lpred gpred)
		(cond 
			((null? ls) (list lpred gpred))
			((pred (car ls) pivot) (partitionList pivot (cdr ls) (cons (car ls) lpred) gpred))
			(else (partitionList pivot (cdr ls) lpred (cons (car ls) gpred))) 
		))))
	(if (< (length ls) 2)
		ls
		(let ((result (partitionList (car ls) (cdr ls) '() '())))
			 (append (qsort pred (car result)) (cons (car ls) (qsort pred (cadr result))))
		)
	)
))

; ---------------------------- Problem 3 ----------------------------
(define (connected? g)
	(letrec ((helper (lambda (g oldEdges replacedSymbol replacedWithSymbol)
				(cond 
					((null? g) '())
					((equal? replacedWithSymbol (car (car g))) (cons (list (car (car g)) (union oldEdges (remove-first replacedSymbol (cadr (car g))))) (helper (cdr g) oldEdges replacedSymbol replacedWithSymbol)))
					((member replacedWithSymbol (cadr (car g))) (cons (list (car (car g)) (remove-first replacedSymbol (cadr (car g)))) (helper (cdr g) oldEdges replacedSymbol replacedWithSymbol)))
					(else (cons (list (car (car g)) (replace replacedSymbol replacedWithSymbol (cadr (car g)))) (helper (cdr g) oldEdges replacedSymbol replacedWithSymbol)))
				)
			)))
	(cond 
		((null? g) #t)
		((null? (cadr (car g))) (eq? 1 (length g)))
		(else (connected? (helper (cdr g) (cdr (cadr (car g))) (car (car g)) (car (cadr (car g))))))
	)
))

; (Below: From 3.ss)
(define (union set1 set2) 
	(if (null? set2)
  		set1
  		(if (member (car set2) set1)
  			(union set1 (cdr set2))
  			(union (cons (car set2) set1) (cdr set2))
  		)))

; (Below: from 5b.ss)
(define (replace old new ls)
	(cond
		((null? ls) '())
		((equal? old (car ls)) (cons new (replace old new (cdr ls))))
		(else (cons (car ls) (replace old new (cdr ls))))
	))

(define (remove-first element ls)
	(cond 
		((null? ls) '())
		((equal? element (car ls)) (cdr ls))
		(else (cons (car ls) (remove-first element (cdr ls))))
	))

; ---------------------------- Problem 4 ----------------------------
(define (reverse-it ls)
	(letrec ((helper (lambda (ls result)
				(if (null? ls)
					result
					(helper (cdr ls) (cons (car ls) result))
			))))
	(helper ls '())
))

; ---------------------------- Problem 5 ----------------------------
(define (empty-BST) '())
(define (empty-BST? obj) (equal? obj (empty-BST)))

(define (BST-element bst) (car bst))
(define (BST-left bst) (cadr bst))
(define (BST-right bst) (caddr bst))

(define (BST-insert num bst)
	(cond
		((empty-BST? bst) (list num (empty-BST) (empty-BST)))
		((< num (BST-element bst)) (list (BST-element bst) (BST-insert num (BST-left bst)) (BST-right bst)))
		((> num (BST-element bst)) (list (BST-element bst) (BST-left bst) (BST-insert num (BST-right bst))))
		(else bst)
	))

(define (BST-insert-nodes bst nums)
	(if (null? nums)
		bst
		(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))
	))

(define (BST-inorder bst)
	(letrec ((helper (lambda (bst acc)
		(if (empty-BST? bst) 
			acc
			(helper (BST-left bst) (cons (BST-element bst) (helper (BST-right bst) acc)))
		)
	)))
	(helper bst '())
))

(define (BST-contains? bst num)
	(cond 
		((empty-BST? bst) #f)
		((eq? (BST-element bst) num) #t)
		((< num (BST-element bst)) (BST-contains? (BST-left bst) num))
		(else (BST-contains? (BST-right bst) num))
	))

(define (BST? obj)
	(letrec ((helper (lambda (modobj)
		(cond 
			((empty-BST? modobj) #t)
			((not (list? modobj)) #f)
			((not (eq? 3 (length modobj))) #f)
			((not (integer? (BST-element modobj))) #f)
			((not (and (helper (BST-left modobj)) (helper (BST-right modobj)))) #f)
			(else (BST-contains? obj (BST-element modobj)))
		))))
	(helper obj)
))