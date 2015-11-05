(define-library (compiler reader lexer)
  (import (scheme base)
	  (compiler utils charset)
	  (compiler utils regex)
	  (compiler reader scanner))
  (export :ascii:

	  |<intraline whitespace>|
	  |<line ending>|
	  |<whitespace>|
	  |<vertical line>|
	  |<comment>|
	  |<directive>|
	  |<atmosphere>|
	  |<intertoken space>|
	  |<identifier>|
	  |<initial>|
	  |<letter>|
	  |<special initial>|
	  |<subsequent>|
	  |<digit>|
	  |<hex digit>|
	  |<explicit sign>|
	  |<special subsequent>|
	  |<inline hex escape>|
	  |<mnemonic escape>|
	  |<peculiar identifier>|
	  |<dot subsequent>|
	  |<sign subsequent>|
	  |<symbol element>|
	  )
  (include "./lexer/r7rs-scheme.scm"))
