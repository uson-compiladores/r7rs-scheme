(import (scheme base)
	(scheme cxr)
	(scheme write))

(define (token-make class lexeme)
  (list ':token: class lexeme))

(define (token? obj)
  (and (pair? obj) (eq? (car obj) ':token)))

(define (token-class tok)
  (cadr tok))

(define (token-lexeme tok)
  (caddr tok))

(define test-input (list (token-make 'open-parentheses "(")
			 (token-make 'int "8")
			 (token-make 'addition "+")
			 (token-make 'int "2")
			 (token-make 'close-parentheses ")")
			 (token-make 'product "*")
			 (token-make 'int "5")))

(define (rule-make name . productions)
  (list ':rule: name productions))

(define (rule? obj)
  (and (pair? obj) (eq? (car obj) ':rule:)))

(define (rule-name rul)
  (cadr rul))

(define (rule-productions rul)
  (caddr rul))

(define (grammar-make initial . rules)
  (list ':grammar: initial rules))

(define (grammar? obj)
  (and (pair? obj) (eq? (car obj) ':grammar:)))

(define (grammar-initial gra)
  (cadr gra))

(define (grammar-rules gra)
  (caddr gra))

(define test-grammar
  (grammar-make 'S
		(rule-make 'S
			   '(E))
		(rule-make 'E
			   '(F addition E)
			   '(F))
		(rule-make 'F
			   '(T product F)
			   '(T))
		(rule-make 'T
			   '(int)
			   '(open-parentheses E close-parentheses))))

(define (grammar-productions gra rulname)
  (let loop ((rules (grammar-rules gra)))
    (cond ((null? rules) #false)
	  ((eq? (rule-name (car rules)) rulname)
	   (rule-productions (car rules)))
	  (else
	   (loop (cdr rules))))))

(for-each (lambda (rule)
	    (display rule)
	    (newline))
	  (grammar-rules test-grammar))

(define (longest-prefix lists)
  (define (loop prefix rests)
    'TODO)
  (loop '() lists))
