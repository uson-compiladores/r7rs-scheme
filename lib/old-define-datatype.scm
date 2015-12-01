(define variant-type-checker&registry-updater '|not yet implemented|)
(define case-checker '|not yet implemented|)

(let ((type-registry '())
      (variant-registry '()))
  (set! variant-type-checker&registry-updater
	(letrec ((set?
		  (lambda (s)
		    (if (null? s) #t
			(and (not (memq (car s) (cdr s))) (set? (cdr s)))))))
	  (lambda (type-name variants)
	    (if (not (symbol? type-name))
		(error "define-variant-type: The variant type is not an identifier" type-name))
	    (for-each
	     (lambda (variant)
	       (if (not (symbol? (car variant)))
		   (error "define-variant-type: The variant-name is not an identifier" type-name (car variant))))
	     variants)
	    (let ((variant-names (map car variants)))
	      (if (not (set? variant-names))
		  (error "define-variant-type: Some of the variant-names are repeated" type-name variant-names))
	      (for-each
	       (lambda (v)
		 (cond ((assq v variant-registry) =>
			(lambda (pair)
			  (if (not (eq? (cdr pair) type-name))
			      (error "define-variant-type: The variant-name has already been used as a variant-name"
				     type-name v (cdr pair)))))))
	       variant-names)
	      (cond ((assq type-name variant-registry) =>
		     (lambda (pair)
		       (error "define-variant-type: The type name has already been used" type-name type-name (car pair) (cdr pair))))
		    ((memq type-name variant-names)
		     (error "define-variant-type: Variant name is the same as the data type name" type-name)))
	      (for-each
	       (lambda (variant-name)
		 (cond
		  ((memq variant-name type-registry)
		   (error "define-variant-type: The variant name has already been used as a type name"
			  type-name variant-name))))
	       variant-names)
	      (set! variant-registry
		    (append
		     (map (lambda (v) (cons v type-name)) variant-names)
		     variant-registry))
	      (cond
	       ((memq type-name type-registry) =>
		(lambda (pair)
		  (set-car! pair type-name)))
	       (else
		(set! type-registry
		      (cons type-name type-registry)))))))))


(define-syntax define-datatype
  (syntax-rules ()
    ((_ type-name)
     (error "define-datatype: There are no variants"
	    '(define-datatype type-name)))
    ((_ type-name type-name?)
     (error "define-datatype: There are no variants"
	    '(define-datatype type-name type-name?)))
    ((_ type-name type-name?
	(variant-name (field-name pred?) ...)
	...)
     (begin
       (define type-name
	 (cons '(variant-name ...)
	       '((variant-name field-name ...) ...)))
       (define type-name?
	 (lambda (variant)
	   (let ((type-info type-name))
	     (if (and (pair? type-info) (list? (car type-info)))
		 (and (pair? variant)
		      (memq (car variant) (car type-info)) #t)
		 (error "dsa")))))

       (define variant-name
	 (let ((expected-length (length '(field-name ...)))
	       (field-names '(field-name ...))
	       (pred-names '(pred? ...))
	       (preds (list (lambda (x) (pred? x)) ...)))
	   (lambda args
	     (if (not (= (length args) expected-length))
		 (error "asdfsadf"))
	     (for-each
	      (lambda (a f p pname)
		(if (not (p a))
		    (error "bad field")))
	      args
	      field-names
	      preds
	      pred-names)
	     (cons 'variant-name args))))
       ...))))

(define-syntax cases
  (syntax-rules ()
    ((_ type-name expression . clauses)
     (let ((type-predicate? (isa type-name)))
       (let ((x expression))
	 (if (type-predicate? x)
	     (case-helper x . clauses)
	     (error "blo blu bla bli")))))))

(define-syntax case-helper
  (syntax-rules (else)
    ((_ variant (else body0 body1 ...))
     (begin body0 body1 ...))
    ((_ variant (purported-variant-name (purported-field-name ...) body0 body1 ...))
     (apply (lambda (purported-field-name ...) body0 body1 ...)
	    (cdr variant)))
    ((_ variant (purported-variant-name (purported-field-name ...) body0 body1 ...) clause ...)
     (if (eq? (car variant) 'purported-variant-name)
	 (apply (lambda (purported-field-name ...) body0 body1 ...)
		(cdr variant))
	 (case-helper variant clause ...)))
    ((_ variant neither-an-else-nor-clause ...)
     (error "bla bla bla"))))
