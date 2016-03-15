; Jacob Knispel & Alec Tiefenthal
(define (largest-in-lists ls) 
	(if (null? (filter pair? ls)) #f
		(apply max (apply append ls))
))