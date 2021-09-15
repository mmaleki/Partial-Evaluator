#lang racket

(define (driver)
  (driver-loop '+ (print '|lithp 1th lightening|)))

(define (driver-loop procedures hunoz)
  (driver-loop-1 procedures (read)))

(define (driver-loop-1 procedures form)
  (cond ((symbol? form)
         (driver-loop procedures (print (eval form '() procedures))))
        ((equal? (car form) 'define)
         (driver-loop (bind (list (caadr form))
                            (list (list (cdadr form) (caddr form)))
                            procedures)
                      (print (caadr form))))
        (#t (driver-loop procedures (print (eval form '() procedures))))))

(define (bind vars args env)
  (cons (list vars args) env))

(define (value name env)
  (value1 name (lookup name env)))

(define (value1 name slot)
  (cond ((equal? slot '&unbound) (error "bad expression"))
        (else slot)))

(define (lookup name env)
  (cadr (assoc name env)))

(define (eval exp env procedures)
  (cond ((symbol? exp)
         (cond ((equal? exp '()) '())
               ((equal? exp #t) #t)
               ((number? exp) exp)
               (else (value exp env))))
        ((equal? (car exp) 'quote)
         (cadr exp))
        ((equal? (car exp) 'cond)
         (evcond (cdr exp) env procedures))
        (else (apply (values (car exp) procedures) (evlis (cdr exp) env procedures) procedures))))

(define (apply fun args procedures)
  (cond ((primop? fun) (primop-apply fun args))
        (else (eval (cadr fun)
                    (bind (car fun) args '())
                    procedures))))

(define (evcond clauses env procedures)
  (cond ((null? clauses) (error))
        ((eval (caar clauses) env procedures)
         (eval (cadar clauses) env procedures))
        (else (evcond (cdr clauses) env procedures))))

(define (evlis arglist env procedures)
  (cond ((null? arglist) '())
        (else (cons (eval (car arglist) env procedures)
                    (evlis (cdr arglist) env procedures)))))

(define (primop? fun)
  (cond ((equal? fun '+) #t)))
    

(define (primop-apply fun args)
  (+ 2 3))


;;------------------------------------- Y combinator-------------
;;--------------------------------------            -------------

(define (y f)
  ((lambda (g)
     (lambda (x) ((f (g g)) x)))
   (lambda (g)
     (lambda (x) ((f (g g)) x)))))

(define (fac k)
  ((y (lambda (f)
        (lambda (n) (if (equal? n 0) 1
                        (* n (f (- n 1))))))) k))

(define fib1
  (lambda (f)
    (lambda (n) (if (< n 2) 1
                    (+ (f (- n 1)) (f (- n 2)))))))

(define fib
  (y fib1))
