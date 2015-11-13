;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Based on Neil Van Dyke's Testeez unit test mechanism
;;;
;;; 

(define-record-type <test-data>
  (%test-data-make title total passed failed)
  test-data?
  (title  test-data-title  set-test-data-title!)
  (total  test-data-total  set-test-data-total!)
  (passed test-data-passed set-test-data-passed!)
  (failed test-data-failed set-test-data-failed!))

(define (tester-make-initial-data title)
  (%test-data-make title 0 0 0))

(define-syntax tester
  (syntax-rules (test-equal test-eval test-define)
    ((tester (x ...) body ...)
     (tester #f (x ...) body ...))
    ((tester title body ...)
     (let ((data (tester-initialize title)))
       (tester-run-tests data body ...)
       (tester-report-tests data)))))

(define (tester-initialize title)
  (newline)
  (display ";;; BEGIN")
  (and title
       (begin (display " ")
	      (write title)))
  (display " TESTS")
  (newline)
  (tester-make-initial-data title))


(define-syntax tester-run-tests
  (syntax-rules (test/eq test/equal test/eqv test-eval test-define)
    (;; first test is an equivalence test with an associated comparator
     (tester-run-tests data
		       (test-equivalence description expression expected comparator)
		       rest ...)
     ;; 
     (begin (tester-start-test data description (quote expression))
	    (let ((result-list   (call-with-values (lambda () expression) list))
		  (expected-list (call-with-values (lambda () expected) list)))
	      (tester-test data
			   comparator
			   result-list
			   expected-list))
	    (tester-run-tests data rest ...)))
    (;; first test is test/eq
     (tester-run-tests data (test/eq description expression expected) rest ...)
     ;;
     (tester-run-tests data
		       (test-equivalence description expression expected eq?)
		       rest ...))
    (;; first test is test/equal
     (tester-run-tests data (test/equal description expression expected) rest ...)
     ;; 
     (tester-run-tests data
		       (test-equivalence description expression expected equal?)
		       rest ...))
    (;; first test is test/eqv
     (tester-run-tests data (test/eqv description expression expected) rest ...)
     ;;
     (tester-run-tests data
		       (test-equivalence description expression expected eqv?)
		       rest ...))
    (;; first test is test-define
     (tester-run-tests data (test-define description variable value) rest ...)
     ;;
     (begin (tester-start-definition description
				     (list 'define (quote variable) (quote value)))
	    (let ()
	      (define variable value)
	      (tester-run-tests data rest ...))))
    (;; first test is test-eval
     (tester-run-tests data (test-eval description expression) rest ...)
     ;;
     (begin (tester-start-evaluation description (quote expression))
	    (let ((result (call-with-values (lambda () expression) list)))
	      (tester-print-values ";; ==> "
				   ";;     "
				   result))
	    (tester-run-tests data rest ...)))
    (;; first test is a quick expression/expected generic check
     (tester-run-tests data (expression expected) rest ...)
     ;;
     (tester-run-tests data (test/equal "" expression expected) rest ...))
    (;; no more test have to be executed (return nothing)
     (tester-run-tests data)
     ;;
     (values))))


(define (tester-start-test data description quoted-expression)
  (set-test-data-total! data (+ 1 (test-data-total data)))
  (newline)
  (display ";; ")
  (display (test-data-total data))
  (display ". ")
  (display description)
  (newline)
  (write quoted-expression)
  (newline))

(define (tester-test data comparator result-list expected-list)
  (define (failed)
    (set-test-data-failed! data (+ 1 (test-data-failed data)))
    (display ";; FAILED! Expected:")
    (newline)
    (tester-print-values ";;     "
			 ";;     "
			 expected-list))
  (tester-print-values ";; ==> "
		       ";;     "
		       result-list)
  (let loop ((result-list   result-list)
	     (expected-list expected-list))
    (if (null? result-list)
	(if (null? expected-list)
	    (begin (set-test-data-passed! data (+ 1 (test-data-passed data)))
		   (display ";; Passed.")
		   (newline))
	    (failed))
	(if (null? expected-list)
	    (failed)
	    (if (comparator (car result-list)
			    (car expected-list))
		(loop (cdr result-list)
		      (cdr expected-list))
		(failed))))))

(define (tester-start-definition description quoted-expression)
  (newline)
  (display ";; DEFINE: ")
  (display description)
  (newline)
  (write quoted-expression)
  (newline))

(define (tester-start-evaluation description quoted-expression)
  (newline)
  (display ";; EVAL: ")
  (display description)
  (newline)
  (write quoted-expression)
  (newline))

(define (tester-print-values first-prefix next-prefix value-list)
  (display first-prefix)
  (if (null? value-list)
      (newline)
      (let loop ((value-list value-list))
	(write (car value-list))
	(newline)
	(or (null? (cdr value-list))
	    (begin (display next-prefix)
		   (loop (cdr value-list)))))))

(define (tester-report-tests data)
  (define title  (test-data-title data))
  (define total  (test-data-total data))
  (define passed (test-data-passed data))
  (define failed (test-data-failed data))
  (newline)
  (display ";;; END")
  (and title
       (begin (display " ")
	      (write title)))
  (display " TESTS: ")
  (display (if (zero? failed) "PASSED" "FAILED"))
  (newline)
  (display ";;;     (Total: ")
  (display total)
  (display "  Passed: ")
  (display passed)
  (display "  Failed: ")
  (display failed)
  (display ")")
  (newline))
