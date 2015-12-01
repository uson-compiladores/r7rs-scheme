;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gramática simplificada de R4RS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <delimiter>    ==> <whitespace> | ( | ) | " | ;
;; <whitespace>   ==> <space or newline>
;; <comment>      ==> ; <all subsequent characters up to a line break>
;; <atmosphere>   ==> <whitespace> | <comment>
;; <intertoken space> ==> <atmosphere>*
(define <whitespace> (rx (c #\space #\newline)))
(define <comment> (rx (.. (c #\;) (* (~ (c #\newline))) (c #\newline))))
(define <atmosphere> (rx (: <whitespace> <comment>)))
(define <intertoken-space> (rx (* <atmosphere>)))
(define <delimiter> (rx (: <whitespace> (c #\( #\) #\" #\;))))

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
(define <digit> digit)
(define <special-initial> (rx (c "!$%&*/:<=>?~_^")))
(define <letter> letter)
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
(define <ascii> ascii)
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
(define-lexer r4rs-lex
  (<identifier>  (lexeme 'IDENTIFIER))
  (<boolean>     (lexeme 'BOOLEAN))
  (<number>      (lexeme 'NUMBER))
  (<character>   (lexeme 'CHARACTER))
  (<string>      (lexeme 'STRING))
  ((rx (c #\())  (lexeme 'RPAREN))
  ((rx (c #\)))  (lexeme 'LPAREN))
  ((rx (s "#(")) (lexeme 'VPAREN))
  ((rx (c #\'))  (lexeme 'QUOTE))
  ((rx (c #\`))  (lexeme 'QUASIQUOTE))
  ((rx (c #\,))  (lexeme 'UNQUOTE))
  ((rx (s ",@")) (lexeme 'UNQUOTE-SPLICING))
  ((rx (c #\.))  (lexeme 'DOT)))

(define (test-r4rs-lex str)
  (stream->list (r4rs-lex (string->scanner str))))
