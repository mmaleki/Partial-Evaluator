(define (before p? xs)
	(if (< (length xs) 2)
		'()
		(if (p? (cadr xs)) 
			(list (car xs))
			(before p? (cdr xs)))))

