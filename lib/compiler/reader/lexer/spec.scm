(define (lexeme->boolean lexeme)
  (if (string=? (substring lexeme 0 2) "#f")
      #false
      #true))

(define (lexeme->character lexeme)
  (let ((str (substring lexeme 2)))
    (if (= 1 (string-length str))
	(car (string->list str))
	(case str
	  (("alarm") #\alarm)
	  (("backspace") #\backspace)
	  (("delete") #\delete)
	  (("escape") #\escape)
	  (("newline") #\newline)
	  (("null") #\null)
	  (("return") #\return)
	  (("space") #\space)
	  (("tab") #\tab)))))


(define initial (rx (: letter (c "!$%&*/:<=>?^_~"))))
(define subsequent (rx (: initial digit (c "+-.@"))))
(define symbol-element (rx (: (~ (c "|\\"))
			      (s "\\a") (s "\\b") (s "\\t") (s "\\n") (s "\\r")
			      (s "\\|"))))
(define peculiar-identifier (rx (: (c "+-")
				   (.. (c "+-") (: initial (c "+-") (c #\@)) (* subsequent))
				   (.. (c "+-") (c #\.) (: initial (c "+-") (c #\@) (c #\.)) (* subsequent))
				   (.. (c #\.) (: initial (c "+-") (c #\@) (c #\.)) (* subsequent)))))
(define string-element (rx (: (~ (c "\"\\"))
			      (s "\\a") (s "\\b") (s "\\t") (s "\\n") (s "\\r")
			      (s "\\\"") (s "\\\\")
			      (.. (c #\\) (* (c " \t")) (c "\n") (* (c " \t"))))))

(define-lexer scm-lex
  ((rx (c #\())   (const '|<(>| ))

  ((rx (c #\)))   (const '|<)>| ))

  ((rx (c #\.))   (const '|<.>| ))

  ((rx (* whitespace)) (const '|<atmosphere>|))
  
  ((rx (: (s "#t") (s "#f") (s "#true") (s "#false")))
   (relate '|<boolean>| lexeme->boolean))

  ((rx (: (.. (s "#\\") ascii)
	  (.. (s "#\\") (: (s "alarm") (s "backspace") (s "delete") (s "escape") (s "newline") (s "null")
			   (s "return") (s "space") (s "tab")))))
   (relate '|<character>| lexeme->character))

  ((rx (: (.. initial (* subsequent))
	  (.. (c #\|) (* symbol-element) (c #\|))
	  peculiar-identifier))
   (relate '|<identifier>| string->symbol))

  ((rx (.. (c #\") (* string-element)  (c #\")))
   (relate '|<string>| (lambda (x) x))))
