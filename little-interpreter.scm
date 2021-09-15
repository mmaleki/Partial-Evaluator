
(define (atom? x)
   (not (pair? x)))

(define eval
  (lambda (exp)
    (if (atom? exp)
	(if (number? exp) exp 'error)
	(cons exp 'a))))

(define fac
  (lambda (x)
    (cond ((= x 0) 1)
	  ((> x 0) (* x (fac (- x 1)))))))

(define Y
  (lambda (f)
    ((lambda (x) (f  (lambda (a) ((x x) a))))
     (lambda (x) (f (lambda (a) ((x x) a))))))) 
   

(define fact
  (Y
   (lambda (f)
     (lambda (n)
       (if (= n 0) 1 (* n (f (- n 1))))))))

(define (equal? x y)
  (cond ((number? x)
	 (cond ((number? y) (= x y))
	       (#t ())))
	((atom? x) (eq? x y))
	((atom? y) ())
	((equal? (car x) (car y))
	 (equal? (cdr x) (cdr y)))))

(define (flist l)
  (cond ((null? l) '())
	(#t (cons (square (car l))
		  (flist (cdr l))))))

(define (apply-list f l)
  (cond ((null? l) '())
	(#t (cons (f (car l))
		  (apply-list f (cdr l))))))

(define (driver)
  (driver-loop '+ (display '|Hellow World|)))

(define (driver-loop proc hunoz)
  (driver-loop-1 proc (read)))


(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

;;Church numerals

(define zero (lambda (f)
	       (lambda (x) x)))

(define one (lambda (f)
	      (lambda (x) (f x))))

(define two (lambda (f)
	      (lambda (x) (f (f x)))))

(define church-succ (lambda (n)
		      (lambda (f)
			(lambda (x) (f ((n f) x))))))

(define church-sum (lambda (m)
		     (lambda (n)
		       (lambda (f)
			 (lambda (x) (m (f ((n f) x))))))))

;;end Church-numerals

(define (searchitem item ls)
  (cond ((null? ls) false)
	((eq? item (car ls)) ls)
	(else (searchitem item (cdr ls)))))

