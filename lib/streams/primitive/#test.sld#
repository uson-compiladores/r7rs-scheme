(define-library (streams primitive test)
  (import (scheme base)
	  (streams primitive)
	  (compiler utils tester))
  (begin
    (tester
     "(stream primitives) implementation SRFI-41 compliance"
     (test-define "stream construction"
		  strm123
		  (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))
     (test/equal "stream-car of strm123"
		 (stream-car strm123) 1)
     (test/equal "stream-car of strm123 stream-cdr"
		 (stream-car (stream-cdr strm123)) 2)
     (test/equal "lazyness of a stream and pair null distinctimon"
		 (stream-pair? (stream-cdr (stream-cons (/ 1 0) stream-null))) #f)
     (test/equal "stream recognizer correctness"
		 (stream? (list 1 2 3)) #f)
     (test-define "stream iterator"
		  iter
		  (stream-lambda (f x) (stream-cons x (iter f (f x)))))
     (test-define "all the natural numbers in a stream"
		  nats
		  (iter (lambda (x) (+ x 1)) 0))
     (test/equal "validate accessing natural numbers"
		 (stream-car (stream-cdr nats)) 1)
     (test-define "recursive stream add procedure"
		  stream-add
		  (stream-lambda (s1 s2) (stream-cons
					  (+ (stream-car s1) (stream-car s2))
					  (stream-add (stream-cdr s1)
						      (stream-cdr s2)))))
     (test-define "even numbers as a stream"
		  evens
		  (stream-add nats nats))
     (test/equal "first even number"
		 (stream-car evens) 0)
     (test/equal "second even number"
		 (stream-car (stream-cdr evens)) 2)
     (test/equal "third even number"
		 (stream-car (stream-cdr (stream-cdr evens))) 4))))
