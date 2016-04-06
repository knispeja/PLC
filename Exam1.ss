; Jacob Knispel
; 3/29/2016
; Exam 1

; ---------------------------- C1 ----------------------------
(define (contains-both? los sym1 sym2)
	(if (and (member sym1 los) (member sym2 los))
		#t
		#f
	))

; ---------------------------- C2 ----------------------------
(define (make-vec-iterator v)
	(define vec-iter-index 0)

	(lambda (msg . args)
		(case msg
			((val) (vector-ref v vec-iter-index))
			((set-val!) (vector-set! v vec-iter-index (car args)))
			((next) (set! vec-iter-index (+ vec-iter-index 1)))
			((prev) (set! vec-iter-index (- vec-iter-index 1)))
		)
	)
)

; ---------------------------- C3 ----------------------------
(define (matrix-sum m1 m2)
	(map (lambda (l1 l2) (map + l1 l2)) m1 m2))

; ---------------------------- C4 ----------------------------

(define (pascal-triangle n)
	(cond
		((< n 0) '())
		((= n 0) '((1)) )
		(else (let ((triangle-n-1 (pascal-triangle (- n 1))))
					(cons (cons 1 (row-helper (car triangle-n-1)))
					triangle-n-1)))))

(define (row-helper prev-row)
	(map + prev-row (append (cdr prev-row) '(0) )))