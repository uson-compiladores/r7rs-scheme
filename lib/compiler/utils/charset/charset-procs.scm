(define (charset-member? x charset)
  (if (charset-node? charset)
      (let ((value (charset-value charset))
	    (left  (charset-left  charset))
	    (right (charset-right charset)))
	(cond ((char<? x value) (charset-member? x left))
	      ((char>? x value) (charset-member? x right))
	      (else #true)))
      (char=? x charset)))

(define (charset-min charset)
  (if (charset-node? charset)
      (let ((value (charset-value charset))
	    (left  (charset-left  charset))
	    (right (charset-right charset)))
	(if (charset-node? left)
	    (charset-min left)
	    value))
      (error "charset-min: not a charset node" charset)))

;;; take a charset with the structure
;;; (a X
;;;    (b Y Z))
;;; and produce a charset with the structure
;;; (b (a X Y)
;;;    Z)
(define (charset-single-left-rotation a x right)
  (let ((b (charset-value right))
	(y (charset-left  right))
	(z (charset-right right)))
    (%charset-make b
		   (%charset-make a x y)
		   z)))

;;; take a charset with the structure
;;; (a X
;;;    (c (b Y1 Y2)
;;;       Z))
;;; and produce a charset with the structure
;;; (b (a X Y1)
;;;    (c Y2 Z))
(define (charset-double-left-rotation a x right)
  (let* ((c    (charset-value right))
	 (left (charset-left  right))
	 (z    (charset-right right))
	 (b    (charset-value left))
	 (y1   (charset-left  left))
	 (y2   (charset-right left)))
    (%charset-make b
		   (%charset-make a x y1)
		   (%charset-make c y2 z))))

;;; take a charset with the structure
;;; (b (a X Y)
;;;    Z)
;;; produce a charset with the structure
;;; (a X
;;;    (b Y Z))
(define (charset-single-right-rotation b left z)
  (let ((a (charset-value left))
	(x (charset-left  left))
	(y (charset-right left)))
    (%charset-make a
		   x
		   (%charset-make b y z))))

;;; take a charset with the structure
;;; (c (a X
;;;       (b Y1 Y2))
;;;    Z)
;;; produce a charset with the structure
;;; (b (a X Y1)
;;;    (c Y2 Z))
(define (charset-double-right-rotation c left z)
  (let* ((a (charset-value left))
	 (x (charset-left left))
	 (right (charset-right left))
	 (b (charset-value right))
	 (y1 (charset-left right))
	 (y2 (charset-right right)))
    (%charset-make b
		   (%charset-make a x y1)
		   (%charset-make c y2 z))))

(define weight 4)

(define (charset-make value left right)
  (let ((left-size (charset-size left))
	(right-size (charset-size right)))
    (cond ((< (+ left-size right-size) 2)
	   (%charset-make value left right))
	  ((> right-size (* weight left-size))
	   (let ((rl (charset-left  right))
		 (rr (charset-right right)))
	     (if (< (charset-size rl)
		    (charset-size rr))
		 (charset-single-left-rotation value left right)
		 (charset-double-left-rotation value left right))))
	  ((> left-size (* weight right-size))
	   (let ((ll (charset-left  left))
		 (lr (charset-right left)))
	     (if (< (charset-size lr)
		    (charset-size ll))
		 (charset-single-right-rotation value left right)
		 (charset-double-right-rotation value left right))))
	  (else
	   (%charset-make value left right)))))

(define (charset-add charset x)
  (if (charset-node? charset)
      (let ((value (charset-value charset))
	    (left  (charset-left  charset))
	    (right (charset-right charset)))
	(cond ((char<? x value)
	       (charset-make value (charset-add left x) right))
	      ((char<? value x)
	       (charset-make value left (charset-add right x)))
	      (else
	       (%charset-make x left right))))
      (%charset-make x '() '())))

(define (charset-delete charset x)
  (if (charset-node? charset)
      (let ((value (charset-value charset))
	    (left  (charset-left  charset))
	    (right (charset-right charset)))
	(cond ((char<? x value)
	       (charset-make value (charset-delete left x) right))
	      ((char<? value x)
	       (charset-make value left (charset-delete right x)))
	      (else
	       (charset-delete* left right))))
      charset))

(define (charset-delete* left right)
  (cond ((charset-null? left)  right)
	((charset-null? right) left)
	(else (charset-make (charset-min right) left (charset-delete-min right)))))

(define (charset-delete-min charset)
  (if (charset-node? charset)
      (let ((value (charset-value charset))
	    (left  (charset-left  charset))
	    (right (charset-right charset)))
	(if (charset-null? left)
	    right
	    (charset-make value (charset-delete-min left) right)))
      (error "charset-delete-min: not a charset node" charset)))
