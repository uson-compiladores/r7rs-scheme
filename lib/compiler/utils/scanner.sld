(define-library (compiler utils scanner)
  (import (scheme base)
	  (scheme file)
	  (streams))
  (export file->scanner
	  string->scanner
	  scanner-read
	  scanner-next)
  (include "./scanner/scanner.scm"))
