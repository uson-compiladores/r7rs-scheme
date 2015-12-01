(define-library (compiler utils lexer)
  (import (scheme base)
	  (scheme write)
	  (streams)
	  (compiler utils scanner)
	  (compiler utils regex))
  (export
   make-rule rule-regex rule-callback
   make-match match-consumed match-rest match-rule
   make-result result-token result-rest

   const
   relate
   lexeme

   rule-map
   next-token
   lexer
   define-lexer

   digit
   whitespace
   ascii
   letter

   scheme-lex

   ;; example
   test-input
   test-lex
   )
  (include "./lexer/lexer.scm"))

