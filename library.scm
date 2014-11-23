
(define (append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1) (append (cdr L1) L2)))
        ))
		
(define (map f L)
   (cond ((null? L) '())
         (else (cons (f (car L)) (map f (cdr L))))))
		 
(define (assoc var list)
	(cond ((null? list) #f)
	      ;((not (pair? list)) #f)
		  ((eq? (car(car list)) var) (car list))
		  (else (assoc var (cdr list)))
		  ))
		  
		  
(define (not x)
    (if x #f #t))
	
(define (cadr L)
	(car (cdr L))
	)
	
	(define (cdar L)
	(cdr (car L))
	)
	

(define (cddr L)
	(cdr (cdr L))
	)
	
	(define (cdddr L)
	(cdr (cdr (cdr L)))
	)

	(define (caddr L)
	(car (cdr (cdr L)))
	)

	(define (cadddr L)
		(car (cdr (cdr (cdr L))))
	)
		
(define (and x y)
  (if x y #f))

(define (or x y)
  (if x #t y))
  
(define (equal? x y)
    (cond ((not(pair? x))  (if (eq? x y) #t #f))
           (else (if (equal? (car x) (car y)) (equal? (cdr x) (cdr y)) #f))))
  
 