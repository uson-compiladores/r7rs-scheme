(import (scheme base))

(define-syntax record
  (syntax-rules ()
    ((record (var ...) val exp ...)
     (apply (lambda (var ...) exp ...) val))))

(define-syntax record-case
  (syntax-rules ()
    ((record-case exp1 (key vars exp2 ...) ... (else exp3 ...))
     (let ((r exp1))
       (cond ((eq? (car r) (quote key))
	      (record vars (cdr r) exp2 ...))
	     ...
	     (else exp3 ...))))))

(define (functional body e)
  (list body e))

(define stack (make-vector 1000))

(define (push! x s)
  (vector-set! stack s x)
  (+ s 1))

(define (index s i)
  (vector-ref stack (- (- s i) 1)))

(define (index-set! s i v)
  (vector-set! stack (- (- s i) 1) v))

(define (compile x e next)
  (cond ((symbol? x)
	 (compile-lookup x e
			 (lambda (n m)
			   (list ':refer: n m next))))
	((pair? x)
	 (record-case x
		      (quote (obj) (list ':constant: obj next))
		      (lambda (vars body) (list ':close:
						(compile body
							 (extend e vars)
							 (list ':return: (+ (length vars) 1)))
						next))
		      (if (test then else)
			  (let ((thenc (compile then e next))
				(elsec (compile else e next)))
			    (compile test e (list ':test: thenc elsec))))
		      (set! (var x)
			    (compile-lookup var e
					    (lambda (n m)
					      (compile x e (list ':assign: n m next)))))
		      (else
		       (let loop ((args (cdr x))
				  (c (compile (car x) e '(:apply:))))
			 (if (null? args)
			     (list ':frame: next c)
			     (loop (cdr args)
				   (compile (car args)
					    e
					    (list ':argument: c))))))))
	(else
	 (list ':constant: x next))))

(define (compile-lookup var e return)
  (let nxtrib ((e e) (rib 0))
    (let nxtelt ((vars (car e)) (elt 0))
      (cond ((null? vars) (nxtrib (cdr e) (+ rib 1)))
	    ((eq? (car vars) var) (return rib elt))
	    (else (nxtelt (cdr vars) (+ elt 1)))))))

(define (extend e r)
  (cons r e))

(define (vm a x e s)
  (record-case x
	       (:halt: () a)
	       (:refer: (n m x)
			(vm (index (find-link n e) m) x e s))
	       (:constant: (obj x)
			   (vm obj x e s))
	       (:close: (body x)
			(vm (functional body e) x e s))
	       (:test: (then else)
		       (vm a (if a then else) e s))
	       (:assign: (n m x)
			(index-set! (find-link n e) m a)
			(vm a x e s))
	       (:frame: (ret x)
			(vm a x e (push! ret (push! e s))))
	       (:argument: (x)
			  (vm a x e (push! a s)))
	       (:apply: ()
		       (record (body link) a
			       (vm a body s (push! link s))))
	       (:return: (n)
			 (let ((s (- s n)))
			   (vm a (index s 0) (index s 1) (- s 2))))
	       (else (error "unknown instruction" (car x)))))

(define (find-link n e)
  (if (= n 0)
      e
      (find-link (- n 1) (index e -1))))

(define (evaluate x)
  (vm '() (compile x '() '(:halt:)) 0 0))
