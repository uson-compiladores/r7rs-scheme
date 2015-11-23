(define-library (streams derived test)
  (import (scheme base)
	  (scheme write)
	  (streams primitive)
	  (streams derived)
	  (compiler utils tester))
  (begin
    (tester
     "(scheme derived) implementation SRFI-41 compliance"
     (test/equal "stream-append"
		 (stream->list
		     (stream-append (stream 1 2 3)
				    (stream 4 5 6)))
		 (list 1 2 3 4 5 6))
     (test/equal "stream-concat"
		 (stream->list
		     (stream-concat
		      (stream (stream 1 2)
			      (stream)
			      (stream 3 4))))
		 (list 1 2 3 4))
     (test/equal "stream-constant"
		 (stream->list 5
		   (stream-constant #t #f))
		 (list #t #f #t #f #t))
     (test/equal "stream-drop & stream-take"
		 (stream->list
		     (stream-append
		      (stream-take 2 (stream 1 2 3 4))
		      (stream-drop 2 (stream 1 2 3 4))))
		 (list 1 2 3 4))
     (test/equal "stream-drop-while"
		 (stream->list (stream-drop-while zero?
						  (stream 0 0 0 1 1 1)))
		 (list 1 1 1))
     (test/equal "stream-filter & stream-from"
		 (stream->list 10 (stream-filter odd? (stream-from 0)))
		 (list 1 3 5 7 9 11 13 15 17 19))
     (test/equal "stream-fold"
		 (stream-fold (lambda (x y) (if (> x y) x y)) 1 (stream 2 3 4 5 6 5 4 3 2 1))
		 6)
     (test/equal "stream-for-each"
		 (stream-for-each display (stream 1 2 3 4))
		 (for-each display (list 1 2 3 4)))
     (test/equal "stream-iterate"
		 (stream->list 10 (stream-iterate (lambda (x) (+ x 1)) 0))
		 (list 0 1 2 3 4 5 6 7 8 9))
     (test/equal "stream-length"
		 (stream-length (stream 1 2 3 4 5))
		 5)
     (test/equal "stream-let"
		 (stream->list (stream-let loop ((strm (stream 1 2 3)))
				 (if (stream-null? strm)
				     stream-null
				     (stream-cons (+ 1 (stream-car strm)) (loop (stream-cdr strm))))))
		 (list 2 3 4))
     (test/equal "stream-map"
		 (stream->list (stream-map (lambda (x) (* x x)) (stream 9 3)))
		 (list 81 9))
     (test/equal "stream-of & stream-range"
		 (stream->list (stream-of (* x x)
				 (x in (stream-range 0 10))
				 (even? x)))
		 (list 0 4 16 36 64))
     (test/equal "stream-ref & stream-scan"
		 (stream-ref
		  (stream-scan * 1 (stream-from 1)) 5)
		 120)
     (test/equal "stream-reverse"
		 (stream->list (stream-reverse (stream 1 2 3 4 5)))
		 (list 5 4 3 2 1))
     (test/equal "stream-take-while"
		 (stream->list (stream-take-while zero?
						  (stream 0 0 0 1 1 1)))
		 (list 0 0 0))
     (test/equal "stream-unfold"
		 (stream->list (stream-unfold
				(lambda (x) (expt x 2))
				(lambda (x) (< x 10))
				(lambda (x) (+ x 1))
				0))
		 '(0 1 4 9 16 25 36 49 64 81))
     (test/equal "stream-unfolds"
		 (call-with-values
		     (lambda ()
		       (stream-unfolds
			(lambda (s)
			  (if (stream-null? s)
			      (values s '() '())
			      (let ((a (stream-car s))
				    (d (stream-cdr s)))
				(if (odd? a)
				    (values d (list a) #f)
				    (values d #f (list a))))))
			(stream-range 1 6)))
		     (lambda (odds evens)
		       (list (stream->list odds)
			     (stream->list evens))))
		 (list (list 1 3 5) (list 2 4)))
     (test/equal "stream-zip"
		 (stream->list (stream-zip (stream-from 0) (stream "cero" "uno" "dos" "tres" "cuatro")))
		 (list (list 0 "cero")
		       (list 1 "uno")
		       (list 2 "dos")
		       (list 3 "tres")
		       (list 4 "cuatro"))))))
