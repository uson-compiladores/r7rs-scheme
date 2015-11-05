(define-record-type <bbtree>
  (%%bbtree-make v c l r)
  bbtree-node?
  (v bbtree-value)
  (c bbtree-count)
  (l bbtree-left)
  (r bbtree-right))

(define (bbtree-components t)
  (values (bbtree-value t)
	  (bbtree-left  t)
	  (bbtree-right t)))

(define bbtree-null '())

(define (bbtree-null? x)
  (null? x))

(define (bbtree-size x)
  (if (bbtree-node? x)
      (bbtree-count x)
      0))

(define (%bbtree-make v l r)
  (%%bbtree-make v (+ 1 (bbtree-size l) (bbtree-size r)) l r))

(define (bbtree-member? x t <)
  (if (bbtree-node? t)
      (let-values (((v l r) (bbtree-components t)))
	(cond ((< x v) (bbtree-member? x l))
	      ((< v x) (bbtree-member? x r))
	      (else    #true)))
      #false))

(define (bbtree-min t)
  (if (bbtree-node? t)
      (let-values (((v l r) (bbtree-components t)))
	(if (bbtree-node? l) (bbtree-min l) v))
      (error "bbtree-min: not a node" t)))

(define (slr a x r)
  (define-values (b y z) (bbtree-components r))
  (%bbtree-make b (%bbtree-make a x y) z))

(define (dlr a x r)
  (define-values (c l z)   (bbtree-components r))
  (define-values (b y1 y2) (bbtree-components l))
  (%bbtree-make b (%bbtree-make a x y1)	(%bbtree-make c y2 z)))

(define (srr b l z)
  (define-values (a x y) (bbtree-components l))
  (%bbtree-make a x (%bbtree-make b y z)))

(define (drr c l z)
  (define-values (a x r)   (bbtree-components l))
  (define-values (b y1 y2) (bbtree-components r))
  (%bbtree-make b (%bbtree-make a x y1) (%bbtree-make c y2 z)))

(define w 4)

(define (bbtree-make v l r)
  (define ls (bbtree-size l))
  (define rs (bbtree-size r))
  (cond ((< (+ ls rs) 2)
	 (%bbtree-make v l r))
	((> rs (* w ls))
	 (let-values (((rv rl rr) (bbtree-components r)))
	   (if (< (bbtree-size rl)
		  (bbtree-size rr))
	       (slr v l r)
	       (dlr v l r))))
	((> ls (* w rs))
	 (let-values (((lv ll lr) (bbtree-components l)))
	   (if (< (bbtree-size lr)
		  (bbtree-size ll))
	       (srr v l r)
	       (drr v l r))))
	(else
	 (%bbtree-make v l r))))

(define (bbtree-add t x <)
  (if (bbtree-node? t)
      (let-values (((v l r) (bbtree-components t)))
	(cond ((< x v)
	       (bbtree-make v (bbtree-add l x <) r))
	      ((< v x)
	       (bbtree-make v l (bbtree-add r x <)))
	      (else
	       (%bbtree-make x l r))))
      (%bbtree-make x '() '())))

(define (bbtree-delete t x <)
  (if (bbtree-node? t)
      (let-values (((v l r) (bbtree-components t)))
	(cond ((< x v)
	       (bbtree-make v (bbtree-delete l x <) r))
	      ((< v x)
	       (bbtree-make v l (bbtree-delete r x <)))
	      (else
	       (bbtree-delete* l r))))
      t))

(define (bbtree-delete* t1 t2)
  (cond ((bbtree-null? t1) t2)
	((bbtree-null? t2) t1)
	(else (bbtree-make (bbtree-min t2) t1 (bbtree-delete-min t2)))))

(define (bbtree-delete-min t)
  (define-values (v l r) (bbtree-components t))
  (if (bbtree-null? l)
      r
      (bbtree-make v (bbtree-delete-min l) r)))

(define (bbtree-inorder-fold f b t)
  (define (fold b t)
    (if (bbtree-node? t)
	(let-values (((v l r) (bbtree-components t)))
	  (fold (f v (fold b r)) l))
	b))
  (fold b t))

(define (bbtree-preorder-fold f b t)
  (define (fold b t)
    (if (bbtree-node? t)
	(let-values (((v l r) (bbtree-components t)))
	  (fold (f v (fold (fold b r) l)) '()))
	b))
  (fold b t))

(define (bbtree-postorder-fold f b t)
  (define (fold b t)
    (if (bbtree-node? t)
	(let-values (((v l r) (bbtree-components t)))
	  (fold (fold (f v b) r) l))
	b))
  (fold b t))

(define (members t) (bbtree-inorder-fold cons '() t))

(define (bbtree-fold f b t)
  (if (bbtree-node? t)
      (let-values (((v l r) (bbtree-components t)))
	(f v (bbtree-fold f b l) (bbtree-fold f b r)))
      b))

(define (bbtree-reverse-add x t <)
  (bbtree-add t x <))

(define (fold f b lst)
  (if (null? lst)
      b
      (f (car lst) (fold f b (cdr lst)))))

(define (list->bbtree lst <)
  (define (reverse-add x xs)
    (bbtree-reverse-add x xs <))
  (fold reverse-add bbtree-null lst))

(define (treesort lst <)
  (members (list->bbtree lst <)))

(define (bbtree-union t1 t2)
  (cond ((bbtree-null? t1) t2)
	((bbtree-null? t2) t1)
	(else
	 (let-values (((v l r) (bbtree-components t)))
	   (bbtree-concat3 v
			   (bbtree-union (bbtree-split-< t1 v)
					 l)
			   (bbtree-union (bbtree-split-> t1 v)
					 r))))))

(define (bbtree-concat3 v l r)
  (cond ((bbtree-null? l) (bbtree-add ))))

(define tree '())
(set! tree (bbtree-add tree 4 <))
(set! tree (bbtree-add tree 9 <))
(set! tree (bbtree-add tree 6 <))
(set! tree (bbtree-add tree 1 <))
(set! tree (bbtree-add tree 8 <))
