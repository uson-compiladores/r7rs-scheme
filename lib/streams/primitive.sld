(define-library (streams primitive)
  (export stream-null stream-cons stream? stream-null? stream-pair?
	  stream-car stream-cdr stream-lambda)
  (import (scheme base)
	  (scheme lazy))
  (include "./primitive/primitive.scm"))
