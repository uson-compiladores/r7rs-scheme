(define-library (compiler utils tester)
  (import (scheme base)
	  (scheme write))
  (export tester)
  (include "./tester/tester.scm"))