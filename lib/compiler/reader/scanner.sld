(define-library (compiler reader scanner)
  (import (scheme base)
	  (scheme file)
	  (scheme lazy))
  (export scanner-make
	  scanner-read
	  scanner-next
	  scanner->list)
  (include "./scanner/scanner.scm"))
