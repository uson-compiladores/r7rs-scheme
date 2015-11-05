(define-record-type <charset>
  (%%charset-make value count left right)
  charset-node?
  (value charset-value)
  (count charset-count)
  (left  charset-left)
  (right charset-right))

(define (charset-null? x)
  (null? x))

(define (charset-size x)
  (if (charset-node? x)
      (charset-count x)
      0))

(define (%charset-make value left right)
  (%%charset-make value
		 (+ 1 (charset-size left) (charset-size right))
		 left
		 right))



