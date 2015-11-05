(define (range x)
  (let recur ((i 0))
    (if (= i x)
	'()
	(cons i (recur (+ i 1))))))

;;;;;;;;;;;;;;
;; CHARSETS ;;
;;;;;;;;;;;;;;

(define :ascii:
  (apply regex-charset (map integer->char (range 127))))

;;;;;;;;;;;;;;;;
;; SUB-TOKENS ;;
;;;;;;;;;;;;;;;;

(define |<intraline whitespace>|
  (rx (c " \t")))

(define |<line ending>|
  (rx (: (c #\newline)
	 (.. (c #\return) (c #\newline))
	 (c #\return))))

(define |<whitespace>|
  (rx (: |<intraline whitespace>|
	 |<line ending>|)))

(define |<vertical line>|
  (rx (c #\|)))

;; The comments in R7RS include the datum comment form ( #; <datum> ) and the
;; nested comment form ( #| <comment text> <comment cont>* |#) wich take into
;; account the matching of open and closing parentheses or the open and closing
;; #| |# sequences. This can't be computed by a machine that generates regular
;; languages like the regular expressions or finite automata. So for now, the
;; comments are going to be made R5RS style.
(define |<comment>|
  (rx (.. (c #\;) (* (~ |<line ending>|)))))

(define |<directive>|
  (rx (: (s "#!fold-case")
	 (s "#!no-fold-case"))))

(define |<atmosphere>|
  (rx (: |<whitespace>|
	 |<comment>|
	 |<directive>|)))

(define |<intertoken space>|
  (rx (* |<atmosphere>|)))

;;;;;;;;;;;;;;;;;
;; IDENTIFIERS ;;
;;;;;;;;;;;;;;;;;

(define |<letter>|
  (rx (charset-union (charset-range #\A #\Z)
		     (charset-range #\a #\z))))

(define |<special initial>|
  (rx (c "!$%&*/:<=>?^_~")))

(define |<initial>|
  (rx (: |<letter>| |<special initial>|)))

(define |<digit>|
  (rx (c "0123456789")))

(define |<explicit sign>|
  (rx (: (c #\+)
	 (c #\-))))

(define |<special subsequent>|
  (rx (: |<explicit sign>|
	 (c #\.)
	 (c #\@))))

(define |<subsequent>|
  (rx (: |<initial>|
	 |<digit>|
	 |<special subsequent>|)))

(define |<mnemonic escape>|
  (rx (: (s "\\a")
	 (s "\\b")
	 (s "\\t")
	 (s "\\n")
	 (s "\\r"))))

(define |<hex digit>|
  (rx (: |<digit>|
	 (c "abcdef"))))

(define |<hex scalar value>|
  (rx (.. |<hex digit>| (* |<hex digit>|))))

(define |<inline hex escape>|
  (rx (.. (s "\\x") |<hex scalar value>| (c #\;))))

(define |<symbol element>|
  (rx (: (~ (: |<vertical line>| (c #\\)))
	 |<inline hex escape>|
	 |<mnemonic escape>|
	 (s "\\|"))))

(define |<sign subsequent>|
  (rx (: |<initial>|
	 |<explicit sign>|
	 (c #\@))))

(define |<dot subsequent>|
  (rx (: |<sign subsequent>|
	 (c #\.))))

(define |<peculiar identifier>|
  (rx (: |<explicit sign>|
	 (.. |<explicit sign>| |<sign subsequent>| (* |<subsequent>|))
	 (.. |<explicit sign>| (c #\.) |<dot subsequent>| (* |<subsequent>|)))))

(define |<identifier>|
  (rx (: (.. |<initial>| (* |<subsequent>|))
	 (.. |<vertical line>| (* |<symbol element>|) |<vertical line>|)
	 |<peculiar identifier>|)))

;;;;;;;;;;;;;;
;; BOOLEANS ;;
;;;;;;;;;;;;;;

(define |<boolean>|
  (rx (: (s "#t")
	 (s "#f")
	 (s "#true")
	 (s "#false"))))

;;;;;;;;;;;;;;;;
;; CHARACTERS ;;
;;;;;;;;;;;;;;;;
(define |<character name>|
  (rx (: (s "alarm")
	 (s "backspace")
	 (s "delete")
	 (s "escape")
	 (s "newline")
	 (s "null")
	 (s "return")
	 (s "space")
	 (s "tab"))))

(define |<character>|
  (rx (: (.. (c #\#) (c #\\) :ascii:)
	 (.. (c #\#) (c #\\) |<character name>|))))
