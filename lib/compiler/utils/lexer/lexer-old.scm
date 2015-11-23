(define (make-rule token regex callback)
  (cons token (cons regex callback)))
(define (rule-token rule) (car rule))
(define (rule-regex rule) (cadr rule))
(define (rule-callback rule) (cddr rule))

(define test-rules
  (list
   (make-rule 'LPAREN (rx (c "(")) (lambda (x) 'LPAREN))
   (make-rule 'RPAREN (rx (c ")")) (lambda (x) 'RPAREN))
   (make-rule 'NUMBER (rx (.. (c "0123456789") (* (c "0123456789")))) (lambda (x) (cons 'NUMBER (string->number (list->string x)))))
   (make-rule 'WHISTESPACE (rx (* (c " \t\n"))) (lambda (x) 'WHITESPACE))))

(define test-input (string->list "(100(300))"))

#;
(define test-rules
  (list
   (make-rule 'UNO (rx (.. (* (c "a")) (c "b"))) (lambda (lexeme) 1))
   (make-rule 'DOS (rx (.. (* (c "a")) (c "c"))) (lambda (lexeme) 2))))

#;
  (define test-input
  (string->list "aaaabhola"))

(define (rule-map rules char)
  (if (null? rules)
      '()
      (let ((d (deriv (rule-regex (car rules)) char)))
	(if (null? d)
	    (rule-map (cdr rules) char)
	    (cons (make-rule (rule-token (car rules))
			     d
			     (rule-callback (car rules)))
		  (rule-map (cdr rules) char))))))

(define (make-match consumed rest rule)
  (cons consumed (cons rest rule)))
(define (match-consumed m) (car m))
(define (match-rest m) (cadr m))
(define (match-rule m) (cddr m))

(define (make-token id lexeme)
  (cons id lexeme))
(define (token-id t) (car t))
(define (token-lexeme t) (cdr t))

(define (next-token rules consumed input longest-match)
  (cond ((null? rules)
	 (if (null? longest-match)
	     '()
	     (cons
	      ((rule-callback (match-rule longest-match)) (reverse (match-consumed longest-match)))
	      (match-rest longest-match))))
	((null? input) ':END:)
	(else
	 (let ((new-rules (rule-map rules (car input))))
	   (next-token new-rules
		       (cons (car input) consumed)
		       (cdr input)
		       (if (and (= (length rules) 1)
				(nullable? (rule-regex (car rules))))
			   (make-match consumed input (car rules))
			   longest-match))))))

(define (lexer . rules)
  ;; return a scanner for the given rules. The scanner gets a stream as input and
  ;; returns a stream as output.
  (lambda (scnr)
    (stream-let loop ((result (next-token rules '() scnr '())))
      (if #t #t #t))

    
    ;(define result (next-token rules '() strm '()))
    ;(define token (car result))
    ;(define rest (cdr result))
    'TODO))



#;
(define (fast-load)
  (load "./lib/compiler/utils/lexer/lexer.scm"))
