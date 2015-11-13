#;
(define (S input productions)
  (cond ((null? productions)
	 #false)
	((try input (car production))
	 #true)
	(else
	 (S input (cdr productions)))))

#;
(define (foo name)
  (define (var input productions)
    (cond ((null? productions)
	   #false)
	  ((try input (car productions))
	   #true)
	  (else
	   (var input (cdr productions))))))
#;
(define (try input production)
  (cond ((null? production))
	((procedure? (car production))
	 ((car production)))
	((char? (car production)))))




(define grammar
  '((S (#\c A #\d))
    (A (#\a #\b)
       (#\a))))

(define (start-var grammar)
  (caar grammar))

(define (call var input grammar)
  )
