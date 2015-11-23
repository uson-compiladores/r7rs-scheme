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

(define (compile x next)
  (cond ((symbol? x)
	 (list ':refer: x next))
	((pair? x)
	 (record-case x
	  (quote (obj) (list ':constant: obj next))
	  (lambda (vars body)
	          (list ':close: vars (compile body '(:return:)) next))
	  (if (test then else)
	      (let ((thenc (compile then next))
		    (elsec (compile else next)))
		(compile test (list ':test: thenc elsec))))
	  (set! (var x)
		(compile x (list ':assign: var next)))
	  (call/cc (x)
		   (let ((c (list ':conti:
				  (list ':argument:
					(compile x '(:apply:))))))
		     (if (tail? next)
			 c
			 (list ':frame: next c))))
	  (else
	   (let loop ((args (cdr x))
		      (c (compile (car x) '(:apply:))))
	     (if (null? args)
		 (if (tail? next)
		     c
		     (list ':frame: next c))
		 (loop (cdr args)
		       (compile (car args)
				(list ':argument: c))))))))
	(else
	 (list ':constant: x next))))

(define (tail? next)
  (eq? (car next) ':return:))

(define (vm a x e r s)
  (record-case x
	       (:halt: () a)
	       (:refer: (var x)
			(vm (car (lookup var e)) x e r s))
	       (:constant: (obj x)
			   (vm obj x e r s))
	       (:close: (vars body x)
			(vm (closure body e vars) x e r s))
	       (:test: (then else)
		       (vm a (if a then else) e r s))
	       (:assign: (var x)
			 (set-car! (lookup var e) a)
			 (vm a x e r s))
	       (:conti: (x)
			(vm (continuation s) x e r s))
	       (:nuate: (s var)
			(vm (car (lookup var e)) '(:return:) e r s))
	       (:frame: (ret x)
			(vm a x e '() (call-frame ret e r s)))
	       (:argument: (x)
			   (vm a x e (cons a r) s))
	       (:apply: ()
			(record (body e vars) a
				(vm a body (extend e vars r) '() s)))
	       (:return: ()
			 (record (x e r s) s
				 (vm a x e r s)))
	       (else (error "unknown instruction" (car x)))))

(define (lookup var e)
  (let nxtrib ((e e))
    (let nxtelt ((vars (caar e)) (vals (cdar e)))
      (cond ((null? vars) (nxtrib (cdr e)))
	    ((eq? (car vars) var) vals)
	    (else (nxtelt (cdr vars) (cdr vals)))))))

(define (closure body e vars)
  (list body e vars))

(define (continuation s)
  (closure (list ':nuate: s 'v) '() '(v)))

(define (call-frame x e r s)
  (list x e r s))

(define (extend e vars vals)
  (cons (cons vars vals) e))

(define (evaluate x)
  (vm '() (compile x '(:halt:)) '() '() '()))

;; (compile '((lambda (x)
;; 	     ((lambda (y)
;; 		x)
;; 	      (set! x 1)))
;; 	   0)
;; 	 '(:halt:))
;; ]=>
;; (:frame: (:halt:)
;; 	    (:constant: 0
;; 		        (:argument: (:close: (x)
;; 					     (:constant: 1
;; 						         (:assign: x
;; 								   (:argument: (:close: (y)
;; 										        (:refer: x
;; 											         (:return:))
;; 										        (:apply:)))))
;; 					     (:apply:)))))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (evaluate '((lambda (x)
;; 	         ((lambda (y)
;; 		    x)
;; 	          (set! x 1)))
;; 	       0))
;; ]=>
;; 1
