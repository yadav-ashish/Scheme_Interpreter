 

(define (repl)     ;;; the read-eval-print loop.
  (display "--> ") 
  (let ((exp (read)))
    (cond ((equal? exp '(exit))      ; (exit) only allowed at top level
	   'done)
	  (else  (display (top-eval exp))
		 (newline)
		 (repl))
	  )))


(define (my-load filename)       ;; don't want to redefine the Scheme LOAD
  (load-repl (open-input-file filename)))


(define (load-repl port)
  (let ((exp (read port)))
    (cond ((eof-object? exp) 'done)
	  (else (let ((res (top-eval exp)))
		  (display res)
		  (load-repl port)))
	  )))



;; insert!, below, is a destructive update of a list L, inserting the
;; parameter val onto the front of L (so that L is actually modified).
;; insert! must only be used where absolutely necessary, e.g. when an
;; environment must be destructively updated to allow for recursion
;; (see the implementation of (define ...) below).

;; As their names imply, set-car! and set-cdr! destructively modify 
;; the car field and cdr field of a cons cell, respectively. They are
;; built-in functions (see *global-env* below).

(define (insert! val L)
  (set-cdr! L (cons (car L) (cdr L)))
  (set-car! L val)
  )


;; (define ....) is only allowed at the top level and affects only the 
;; global environment. Only the basic form of define is supported here.

(define (top-eval exp)
  (cond ((not (pair? exp)) (my-eval exp *global-env*))
	((eq? (car exp) 'define)   
         (cond ((pair? (cadr exp)) (insert! (append (list (car (cadr exp))) (list (my-eval (cons 'lambda (cons (cdr (cadr exp)) (cddr exp))) *global-env*))) *global-env*)) 
	 (else (insert! (list (cadr exp) (my-eval (caddr exp) *global-env*)) *global-env*)
	 (cadr exp)))) ; just return the symbol being defined
	(else (my-eval exp *global-env*))
	))


(define (lookup var env)
  (let ((item (assoc var env)))  ;; assoc returns #f if var not found in env
    (cond ((not item) (display "Error: Undefined Symbol ")
		      (display var) (newline))
	  (else (cadr item))
	  )))


	  
	  

(define new-env '())

	
(define (handle-let* defs body env)
   ;     (display defs)
  ;(display env)
	(cond ((null? defs) (handle-block body env))
	;((pair? (cdar defs)) (handle-let (cdr defs) body (append env (append (list (caar defs)) (list (my-eval (cdar defs) env)) ))))
	(else ;(display (list (car (car defs))))
              ;(display (list (my-eval (cadr (car defs)) env)))
              ;(display (append (list (car (car defs))) (list (my-eval (cadr (car defs)) env))))
         (handle-let* (cdr defs) body (append (list (append (list (car (car defs))) (list (my-eval (cadr (car defs)) env)))) env)))
	))	  

(define (handle-let defs body new-env env)
    ;    (display defs)
  ;(newline)
  ;(display new-env)
  ;(newline)
	(cond ((null? defs) (handle-block body (append new-env env)))
	;((pair? (cdar defs)) (handle-let (cdr defs) body (append env (append (list (caar defs)) (list (my-eval (cdar defs) env)) ))))
	(else ;(display (list (car (car defs))))
              ;(display (list (my-eval (cadr (car defs)) env)))
              ;(display (append (list (car (car defs))) (list (my-eval (cadr (car defs)) env))))
         (handle-let (cdr defs) body (append (list (append (list (car (car defs))) (list (my-eval (cadr (car defs)) env)))) new-env) env))
	))

	
(define (handle-if test then-exp else-exp env)
  (if (my-eval test env)
      (my-eval then-exp env)
      (my-eval else-exp env)))

(define (handle-cond exp1 env)
     (if (null? exp1) '()
         (if (eq? (car (car exp1)) 'else) (handle-block (cdr (car exp1)) env)
                (if (my-eval (car (car exp1)) env) (handle-block (cdr (car exp1)) env) (handle-cond (cdr exp1) env))
         )))
         
;; still missing let, let*, letrec, the syntax for (define (f x) ...),
;; cond, begin (block).

(define (my-eval exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if)
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda)
    (list 'closure exp env))
	((eq? (car exp) 'let)
	 (handle-let (cadr exp) (cddr exp) new-env env))
        ((eq? (car exp) 'load)
	 (my-load (car (cdr exp))))
	((eq? (car exp) 'let*)
	 (handle-let* (cadr exp) (cddr exp) env))
        ((eq? (car exp) 'define1)
	 (handle-define1 exp env))
        ((eq? (car exp) 'cond)
	 (handle-cond (cdr exp) env))
   ((eq? (car exp) 'letrec)
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))


(define (bind formals actuals)
  (cond ((null? formals) '())
	(else (cons (list (car formals) (car actuals))
		    (bind (cdr formals) (cdr actuals))))
	))

(define (handle-block block env)
  (cond ((null? block) (display "Error: Can't have empty block or body"))
	((null? (cdr block)) (my-eval (car block) env))
	(else (my-eval (car block) env)
	      (handle-block (cdr block) env))
	))
    

; Here's how handle-letrec should implement LETREC
; 0) The parameters are the defs,(e.g. ((f exp1) (g exp2)), and the body,
;    which is a list of expressions, e.g. ((display x) (f (g 1)))
; 1) create an association list binding the new names introducted by
;    the letrec to uninitialized values (e.g. the symbol '*uninitialized*).
;    For example, if the new names are x and y, then create 
;    ((x *uninitialized*) (y *uninitialized*))
; 2) create a new-env by appending the above association list to env.
; 3) eval the right hand side of each def using new-env
; 4) destructively modify new-env to replace the unitialized value for each
;    new name with its correspondinng value.
; 5) evaluate the body of the letrec using new-env

(define new-env1 '())
(define new-env2 '())
(define new-env3 '())

(define (handle-letrec defs body env)
       ;(define new-env2 (fun defs new-env1 env))
        ;(display new-env2)
        ;(display 'good-boy)
  ;(newline)
  ;(newline)
  ;(display new-env3)
        (handle-block body (fun2 (fun defs new-env1 env) defs)))


(define (fun val new-env1 env)
  (cond ((null? val) (append new-env1 env))
        (else (fun (cdr val) (append (list (list (car (car val)) (list '*uninitialized*))) new-env1) env))
        ))

(define (look var L)
  (cond ((eq? (car (car L)) var) (car L))
        (else (look var (cdr L)))
        ))
        
(define (fun2 new-env2 defs)
  (cond ((null? defs) new-env2)
        ;
        (else (set-car! (cdr (look (car (car defs)) new-env2)) (my-eval (cadr (car defs)) new-env2)) (fun2 new-env2 (cdr defs)))
        )) 



(define (handle-call evald-exps)
  (let ((fn (car evald-exps))
	(args (cdr evald-exps)))
   (cond
    ((eq? (car fn) 'closure)
     (let ((formals (cadr (cadr fn)))
	   (body (cddr (cadr fn)))
	   (env (caddr fn)))
       (handle-block body (append (bind formals args) env))))
    ((eq? (car fn) 'primitive-function)
     (apply (cadr fn) args))
    (else (display "Error: Calling non-function"))
    )))


(define (my-apply fn args)
  (cond ((eq? (car fn) 'primitive-function) 
         (apply (cadr fn) args))
        (else 
         (let ((formals (cadr (cadr fn)))
	   (body (cddr (cadr fn)))
	   (env (caddr fn)))
       (handle-block body (append (bind formals args) env))))
        ))


;;-------------------- Here is the initial global environment --------

(define *global-env*
  (list (list 'car (list 'primitive-function car))
	(list 'cdr (list 'primitive-function cdr))
	(list 'set-car! (list 'primitive-function set-car!))
	(list 'set-cdr! (list 'primitive-function set-cdr!))
	(list 'cons (list 'primitive-function cons))
	(list 'list (list 'primitive-function list))
	(list '+ (list 'primitive-function +))
	(list '- (list 'primitive-function -))
	(list '* (list 'primitive-function *))
	(list '= (list 'primitive-function =))
	(list '< (list 'primitive-function <))
	(list '> (list 'primitive-function >))
	(list '<= (list 'primitive-function  <=))
	(list '>= (list 'primitive-function >=))
	(list 'eq? (list 'primitive-function eq?))
	(list 'pair? (list 'primitive-function pair?))
	(list 'symbol? (list 'primitive-function symbol?))
	(list 'null? (list 'primitive-function null?))
	(list 'read (list 'primitive-function read))
	(list 'display (list 'primitive-function  display))
	(list 'open-input-file (list 'primitive-function open-input-file))
	(list 'close-input-port (list 'primitive-function close-input-port))
	(list 'eof-object? (list 'primitive-function eof-object?))
	(list 'load (list 'primitive-function my-load))
    (list 'apply (list 'primitive-function my-apply));;defined above
    (list 'newline (list 'primitive-function newline))
	))