(define-record-type <rule>
  (make-rule regex callback)
  rule?
  (regex    rule-regex)
  (callback rule-callback))

(define-record-type <match>
  (make-match consumed rest rule)
  match?
  (consumed match-consumed)
  (rest     match-rest)
  (rule     match-rule))

(define-record-type <result>
  (make-result token rest)
  result?
  (token result-token)
  (rest  result-rest))

(define (rule-map rules char)
  (if (not (null? rules))
      (let ((d (deriv (rule-regex (car rules)) char)))
	(if (null? d)
	    (rule-map (cdr rules) char)
	    (cons (make-rule d
			     (rule-callback (car rules)))
		  (rule-map (cdr rules) char))))
      '()))

(define (next-token rules consumed scanner longest-match)
  (cond ((null? rules)
	 (cond ((match? longest-match)
		(make-result ((rule-callback (match-rule longest-match))
			   (list->string (reverse (match-consumed longest-match))))
			  (match-rest longest-match)))
	       ((stream-null? scanner) stream-null)
	       (else
		(error "next-token: no matching rules" consumed))))
	((stream-null? scanner)
	 (next-token '()
		     consumed
		     scanner
		     (if (and (= (length rules) 1)
			      (nullable? (rule-regex (car rules))))
			 (make-match consumed scanner (car rules))
			 longest-match)))
	(else
	 (let ((new-rules (rule-map rules (scanner-read scanner))))
	   (next-token new-rules
		       (cons (scanner-read scanner) consumed)
		       (scanner-next scanner)
		       (if (and (= (length rules) 1)
				(nullable? (rule-regex (car rules))))
			   (make-match consumed scanner (car rules))
			   longest-match))))))

(define (lexer . rules)
  (lambda (scanner)
    (stream-let loop ((result (next-token rules '() scanner '())))
      (if (not (result? result))
	  stream-null
	  (stream-cons (result-token result)
		       (loop (next-token rules '() (result-rest result) '())))))))

(define test-input
  (list->stream (string->list "(1 (2 3) 4 ((5)))")))

(define (const x)
  (lambda (y) x))

(define (relate x f)
  (lambda (y) (cons x (f y))))

(define test-lex
  (lexer
   (make-rule (rx (c "("))
	      (const 'LPAREN))
   (make-rule (rx (c ")"))
	      (const 'RPAREN))
   (make-rule (rx (.. (c "0123456789") (* (c "0123456789"))))
	      (relate 'NUMBER string->number))
   (make-rule (rx (* (c " \t\n")))
	      (const 'WHITES))))

(define-syntax define-lexer
  (syntax-rules ()
    ((define-lexer lex-name (pat1 call1) (pat2 call2) ...)
     (define lex-name
       (lexer
	(make-rule pat1 call1)
	(make-rule pat2 call2)
	...)))))


(define digit (rx (c "0123456789")))
(define letter (rx (c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(define whitespace (rx (c " \t\n")))
(define ascii (rx (: digit letter whitespace (c "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))))

(define-lexer scheme-lex
  ((rx (.. digit (* digit)))  (relate 'NUMBER string->number))
  ((rx (.. (c #\") (* (: (~ (c #\")) (.. (c #\\) (~ (c ""))))) (c #\"))) (relate 'STRING (lambda (x) x)))
  ((rx (.. (s "#\\") ascii)) (relate 'CHARACTER (lambda (x) (string-ref x 2))))
  ((rx (: (s "#f") (s "#false") (s "#t") (s "#true"))) (lambda (x) (cons 'BOOLEAN (if (char=? (string-ref x 1) #\f)
										      #f #t))))
  ((rx (c "(")) (const 'LPAREN))
  ((rx (c ")")) (const 'RPAREN))
  ((rx (* whitespace)) (const 'WHITES)))
