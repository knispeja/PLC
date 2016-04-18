; Jacob Knispel
; Assignment 11b

; ---------------------------- Problem 4 ----------------------------
(load "chez-init.ss")

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define 2nd-onward cdr)
(define 3rd-onward cddr)
(define 4th-onward cdddr)

(define (parse-exp datum)
	(cond
	     ((symbol? datum) (var-exp datum))
	     ((number? datum) (lit-exp datum))
	     ((vector? datum) (vec-exp datum))
	     ((null? datum) (eopl:error 'parse-exp "Empty Expression ~s" datum))
	     ((list? datum) (case (1st datum)
	     					('lambda (parse-lambda-exp datum))
	     					('set! (parse-set!-exp datum))

	     					('let (parse-let-exp datum))
	     					('let* (parse-let-exp datum))
	     					('letrec (parse-let-exp datum))

	     					('if (parse-if-exp datum))
	       					(else (app-exp (map parse-exp datum)))
	       				))
	     (else (eopl:error 'parse-exp "Bad Expression: ~s" datum))
 	)
)

(define (parse-lambda-exp datum)
	(cond 
		((< (length datum) 3) (eopl:error 'parse-exp "Not enough args to lambda expression ~s" datum))
		((and (list? (2nd datum)) (not (andmap symbol? (2nd datum)))) (eopl:error 'parse-exp "Arguments to lambda expression must be symbols ~s" datum))
		(else 
				(let ((parsed-bodies (map parse-exp (3rd-onward datum))))	
					(if (list? (2nd datum))
						(lambda-exp (2nd datum) parsed-bodies)
						(lambda-exp-variable (2nd datum) parsed-bodies)
					)
				))
	)
)

(define (parse-if-exp datum)
	(cond
		((< (length datum) 3) (eopl:error 'parse-exp "Not enough args to if statement ~s" datum))
		((eq? (length datum) 4) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum))))
		((eq? (length datum) 3) (if-sans-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))))
		(else (eopl:error 'parse-exp "Too many args to if statement ~s" datum))
	)
)

(define (parse-set!-exp datum)
	(cond
		((< (length datum) 3) (eopl:error 'parse-exp "Not enough args to set! ~s" datum))
		((> (length datum) 3) (eopl:error 'parse-exp "Too many args to set! ~s" datum))
		((not (symbol? (2nd datum))) (eopl:error 'parse-exp "First argument to set! must be a symbol ~s" datum))
		(else (set!-exp (2nd datum) (parse-exp (3rd datum))))
	)
)

(define (parse-let-exp datum)
	(cond 
		((< (length datum) 3) (eopl:error 'parse-exp "Not enough args to let expression ~s" datum))
		((and (symbol? (2nd datum)) (list? (3rd datum))) ;Named let
			(if (not (andmap is-list-of-length-2 (3rd datum))) 
				(eopl:error 'parse-exp "Bad let expression - sublists not of length 2 ~s" datum)
				(let ((toBind (map 1st (3nd datum))))
					(cond 
						((not (andmap symbol? toBind)) (eopl:error 'parse-exp "Let expression expressions must bind to symbols ~s" datum))
						((< (length datum) 4) (eopl:error 'parse-exp "Not enough args to named let expression ~s" datum))
						(else 
							(let-exp 
								(1st datum) 
								toBind
								(map parse-exp (map 2nd (3nd datum)))
								(map parse-exp (4th-onward datum))
							))
					)
				)))
		((list? (2nd datum))
			(if (not (andmap is-list-of-length-2 (2nd datum)))
				(eopl:error 'parse-exp "Bad let expression - sublists not of length 2 ~s" datum)
				(let ((toBind (map 1st (2nd datum))))
					(if (not (andmap symbol? toBind))
						(eopl:error 'parse-exp "Let expression expressions must bind to symbols ~s" datum)
						(let-exp 
							(1st datum) 
							toBind
							(map parse-exp (map 2nd (2nd datum)))
							(map parse-exp (3rd-onward datum))
						)
					)
				)
			))
		(else (eopl:error 'parse-exp "Bad let expression! ~s" datum))
	)
)

(define (is-list-of-length-2 ls)
	(and (list? ls) (eq? (length ls) 2)))

(define-datatype expression expression?
	(var-exp (id symbol?))
	
	(lit-exp (id number?))

	(vec-exp (id vector?))

	(lambda-exp (args (list-of symbol?))
				(bodies (list-of expression?)))
	(lambda-exp-variable (args symbol?)
						 (bodies (list-of expression?)))

	(set!-exp (var symbol?)
			  (expr expression?))

	(let-exp (type symbol?)
			 (to-bind (list-of symbol?))
			 (bound-to (list-of expression?))
			 (bodies (list-of expression?)))
	(named-let-exp (type symbol?)
				   (name symbol?)
				   (to-bind (list-of symbol?))
				   (bound-to (list-of expression?))
				   (bodies (list-of expression?)))

	(if-exp (predicate expression?)
			(consequent expression?)
			(alternate expression?))
	(if-sans-else-exp (predicate expression?)
					  (consequent expression?))

	(app-exp
			(exps (list-of expression?)))
)

(define unparse-exp
	(lambda (exp)
		(cases expression exp
			(var-exp (id) id)

			(lit-exp (id) id)

			(vec-exp (id) id)

			(lambda-exp (args bodies) (append (list 'lambda args) (map unparse-exp bodies)))
			(lambda-exp-variable (args bodies) (append (list 'lambda args) (map unparse-exp bodies)))

			(set!-exp (var expr) (list 'set! var (unparse-exp expr)))

			(let-exp (type to-bind bound-to bodies) 
				(append (list 
					type 
					(cross-lists to-bind (map unparse-exp bound-to))
				) (map unparse-exp bodies)))
			(named-let-exp (type name to-bind bound-to bodies)
				(append (list
					type
					name
					(cross-lists to-bind (map unparse-exp bound-to))
				) (map unparse-exp bodies)))

			(if-exp (pred conseq alt) (list 'if (unparse-exp pred) (unparse-exp conseq) (unparse-exp alt)))
			(if-sans-else-exp (pred conseq) (list 'if (unparse-exp pred) (unparse-exp conseq)))

			(app-exp (exps) (map unparse-exp exps))
		)
	)
)

; e.g (1 1 1 2 2 2) (3 3 3 4 4 4) -> ((1 3) (1 3) (1 3) (2 4) (2 4) (2 4))
(define (cross-lists ls1 ls2) 
	(map (lambda (x y) (list x y)) ls1 ls2))