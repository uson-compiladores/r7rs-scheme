;;; A lexicographic analizer generator
;;;
;;; the input consists of three sets
;;; * rules
;;;     a list with elements of the form (regex . procedure) where regex is a regular expression created
;;;     with the rx special form and procedure is a lambda expression that takes a lexeme and returns a
;;;     token.
;;; * delimiters
;;;     a list of characters that will act as delimiters in the analisis of the input, that is, when a
;;;     delimeter is found on the input, the partial match will be completed and the input will not be
;;;     consumed if and only if the partial match regex is nullable.
;;; * intertoken-space
;;;     a regular expression that if there isn't a partial match will consume the longest match from the
;;;     input stream without tokenization.
;;;
;;; A simple reduced but R4RS compliant lexical structure is used as an example

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
	(else (filter pred (cdr lst)))))

(define (make-rule regex op) (cons regex op))
(define (rule-regex r) (car r))
(define (rule-op r) (cdr r))

(define (make-partial-match regex stack) (cons regex stack))
(define (partial-match-regex pm) (car pm))
(define (partial-match-stack pm) (cdr pm))

;;; next-token : ((regex . op) ...) x (char ...) x regex x stream -> (obj . stream)
(define (next-token rules delims atmosphere scanner)
  (if (stream-null? scanner)
      stream-null
      (let ((c (scanner-read scanner))
	    (x (scanner-next scanner)))
	(if (not (null? (deriv atmosphere c)))
	    (next-token rules delims atmosphere (consume atmosphere scanner))
	    (let loop ((partial-rules rules)
		       (partial-match '())
		       (partial-rest  scanner))
	      (if (stream-null? partial-rest)
		  (if (and (not (null? partial-match))
			   (not (null? partial-rules))
			   (nullable? (rule-regex (car partial-rules))))
		      (cons (build-token (car partial-rules) partial-match)
			    partial-rest)
		      (error "next-token : error 1"))
		  (if (null? partial-rules)
		      (error "next-token : error 2")
		      (let ((c (scanner-read partial-rest))
			    (x (scanner-next partial-rest)))
			(if (and (not (null? partial-match))
				 (not (null? partial-rules))
				 (nullable? (rule-regex (car partial-rules)))
				 (or (member c delims)
				     (member (car partial-match) delims)))
			    (cons (build-token (car partial-rules) partial-match)
				  partial-rest)
			    (loop (filter (lambda (rule) (not (null? (rule-regex rule))))
					  (map (lambda (rule) (make-rule (deriv (rule-regex rule) c)
									 (rule-op rule)))
					       partial-rules))
				  (cons c partial-match)
				  x))))))))))

(define (next-token-debug rules delims atmosphere scanner)
  (display "======================================================================\n")
  (display "next-token-debug\n\trules: ")
  (display (length rules))
  (display "\n    delims: ")
  (display delims)
  (display "\n    atmosphere: ")
  (display (regex->list atmosphere))
  (display "\n    scanner: ")
  (display (stream->list scanner))
  (display "\n")
  (if (stream-null? scanner)
      (begin (display "  scanner is null\n")
	     stream-null)
      (let ((c (scanner-read scanner))
	    (x (scanner-next scanner)))
	(display "  scanner is ") (display c) (display ":") (display (stream->list x)) (display "\n")
	(if (not (null? (deriv atmosphere c)))
	    (begin
	      (display "  atmosphere detected... it will be consumed\n")
	      (next-token-debug rules delims atmosphere (consume atmosphere scanner)))
	    (begin
	      (display "  missing atmosphere... must be something tokenable\n")
	      (let loop ((partial-rules rules)
			 (partial-match '())
			 (partial-rest  scanner)
			 (i 1))
		(begin
		  (display "  loop ") (display i)
		  (display "\n      partial-rules: ") (display (length partial-rules))
		  (display "\n      partial-match: ") (display (reverse partial-match))
		  (display "\n      partial-rest: ") (display (stream->list partial-rest))
		  (display "\n")
		  (if (stream-null? partial-rest)
		      (begin
			(display "    partial-rest is null\n")
			(if (and (not (null? partial-match))
				 (not (null? partial-rules))
				 (nullable? (rule-regex (car partial-rules))))
			    ;; build token from partial-match and return it with the partial-rest
			    (begin
			      (display "    partial-match is a token\n")
			      (cons (build-token (car partial-rules) partial-match)
				    partial-rest))
			    (begin
			      (display "    partial-match is not a token... signaling error\n")
			      (error "next-token-debug : error 1"))))
		      (begin
			(display "    partial-rest isn't null\n")
			(if (null? partial-rules)
			    (begin
			      (display "    no rules to apply... signaling error\n")
			      (error "next-token-debug: error 2"))
			    (let ((c (scanner-read partial-rest))
				  (x (scanner-next partial-rest)))
			      (begin
				(display "    partial-rest is ") (display c) (display ":") (display (stream->list x))
				(display "\n")
				(if (and (not (null? partial-match))
					 (not (null? partial-rules))
					 (nullable? (rule-regex (car partial-rules)))
					 (or (member c delims)
					     (member (car partial-match) delims)))
				    ;; build token from partial-match and return it with the partial-rest
				    (begin
				      (display "    partial-match is a token\n")
				      (cons (build-token (car partial-rules) partial-match)
					    partial-rest))
				    (begin
				      (display "    partial-match isn't a token... proceeding tokenization\n")
				      (loop (filter (lambda (rule) (not (null? (rule-regex rule))))
						    (map (lambda (rule) (make-rule (deriv (rule-regex rule) c)
										   (rule-op rule)))
							 partial-rules))
					    (cons c partial-match)
					    x
					    (+ i 1))))))))))))))))

;;; consume : regex x stream -> stream
(define (consume regex scanner)
  (if (stream-null? scanner)
      stream-null
      (let ((regex (deriv regex (scanner-read scanner))))
	(if (null? regex)
	    scanner
	    (consume regex (scanner-next scanner))))))

;;; build-token : (regex . op) x (char ...) -> obj
(define (build-token rule match)
  ((rule-op rule) (list->string (reverse match))))

;;; make-lexer : ((regex . proc) ...) x (char ...) x regex -> (stream -> stream)
(define (make-lexer rules delims atmosphere)
  (lambda (scanner)
    (stream-let loop ((result (next-token rules delims atmosphere scanner)))
      (cond ((stream-null? result) stream-null)
	    ((pair? result) (stream-cons (car result)
					 (loop (next-token rules delims atmosphere (cdr result)))))
	    (else (error "unexpected result from next-token"))))))

(define (make-lexer-debug rules delims atmosphere)
  (lambda (scanner)
    (stream-let loop ((result (next-token-debug rules delims atmosphere scanner))
		      (i 0))
      (display "loop ") (display i) (display "\n")
      (if (begin
	    (display "  at (stream-null? result)\n")
	    (stream-null? result))
	  (begin
	    (display "  at stream-null\n")
	    stream-null)
	  (begin
	    (display "  at (stream-cons (car result) (loop (next-token rules delims atmosphere (cdr result))))\n")
	    (stream-cons (car result)
			 (loop (next-token-debug rules delims atmosphere (cdr result)) (+ i 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; R4RS SAMPLE GRAMMAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; delimiters #\space #\newline #\( #\) #\" #\;

;; <whitespace>   ==> <space or newline>
;; <comment>      ==> ; <all subsequent characters up to a line break>
;; <atmosphere>   ==> <whitespace> | <comment>
;; <intertoken space> ==> <atmosphere>*
(define <whitespace> (rx (c #\space #\newline)))
(define <comment> (rx (.. (c #\;) (* (~ (c #\newline))) (c #\newline))))
(define <atmosphere> (rx (: <whitespace> <comment>)))
(define <intertoken-space> (rx (* <atmosphere>)))

;; <identifier>   ==> <initial> <subsequent>*
;;                    | <peculiar identifier>
;; <initial>      ==> <letter> | <special initial>
;; <letter>       ==> a | b | c | ... | z
;; <special initial> ==> ! | $ | % | & | * | / | : | < | =
;;                    | > | ? | ~ | _ | ^
;; <subsequent>   ==> <initial> | <digit>
;;                    | <special subsequent>
;; <digit>        ==> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;; <special subsequent> ==> . | + | -
;; <peculiar identifier> ==> + | - | ...
(define <peculiar-identifier> (rx (: (c #\+ #\-)
				     (s "..."))))
(define <special-subsequent> (rx (c #\+ #\- #\.)))
(define <digit> (rx (c "0123456789")))
(define <special-initial> (rx (c "!$%&*/:<=>?~_^")))
(define <letter> (rx (c "qwertyuioplkjhgfdsazxcvbnmQWERTYUIOPLKJHGFDSAZXCVBNM")))
(define <initial> (rx (: <letter> <special-initial>)))
(define <subsequent> (rx (: <initial> <digit> <special-subsequent>)))
(define <identifier> (rx (: (.. <initial> (* <subsequent>))
			    <peculiar-identifier>)))

;; <number>       ==> <sign><digit>+
;; <sign>         ==> - | + | <empty>
(define <sign> (rx (: (c #\- #\+) e)))
(define <number> (rx (.. <sign>
			 <digit>
			 (* <digit>))))

;; <boolean>      ==> #t | #f
;; <character>    ==> #\ <any character>
;;                    | #\ <character name>
;; <character name> ==> space | newline
(define <boolean> (rx (.. (c #\#)
			  (c #\t #\f))))
(define <character-name> (rx (: (s "space")
				(s "newline"))))
(define <ascii> regex-alphabet)
(define <character> (rx (.. (c #\#) (c #\\)
			    (: <character-name>
			       <ascii>))))


;; <string>       ==> " <string element>* "
;; <string element> ==> <any character other than " or \>
;;                    | \" | \\
(define <string-element> (rx (: (~ (c #\" #\\))
				(.. (c #\\) (c #\"))
				(.. (c #\\) (c #\\)))))
(define <string> (rx (.. (c #\")
			 (* <string-element>)
			 (c #\"))))

;; <token>        ==> <identifier> | <boolean> | <number>
;;                    | <character> | <string>
;;                    | ( | ) | #( | ' | ` | , | ,@ | .


(define (tag x)
  (lambda (y) (cons x y)))

(define (const x)
  (lambda (y) x))

(define r4rs-rules
  (list
   (make-rule (rx (s "("))   (const '<lparen>))
   (make-rule (rx (s ")"))   (const '<rparen>))
   (make-rule (rx (s "#("))  (const '<vparen>))
   (make-rule (rx (s "'"))   (const '<quote>))
   (make-rule (rx (s "`"))   (const '<quasiquote>))
   (make-rule (rx (s ","))   (const '<unquote>))
   (make-rule (rx (s ",@"))  (const '<unquote-splicing>))
   (make-rule (rx (s "."))   (const '<cons-dot>))
   (make-rule <identifier>   (tag '<identifier>))
   (make-rule <boolean>      (tag '<boolean>))
   (make-rule <number>       (tag '<number>))
   (make-rule <character>    (tag '<character>))
   (make-rule <string>       (tag '<string>))))

(define r4rs-delims
  (list #\space #\newline #\( #\) #\" #\;))

(define r4rs-atmosphere <intertoken-space>)

(define r4rs-lexer (make-lexer r4rs-rules r4rs-delims r4rs-atmosphere))
(define r4rs-lexer-debug (make-lexer-debug r4rs-rules r4rs-delims r4rs-atmosphere))
