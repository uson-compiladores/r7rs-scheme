;;; tree datatype example definition ;;;
;; (define-datatype tree tree?        ;;
;;   (leaf (weight integer?))         ;;
;;   (node (weight integer?)          ;;
;; 	   (left   tree?)             ;;
;; 	   (right  tree?)))           ;;
;;                                    ;;
;; (define t (node 3                  ;;
;;                 (leaf 1)           ;;
;;                 (leaf 1)))         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype type-name)
     (syntax-error "There are no variants" type-name))
    ((define-datatype type-name type-name?)
     (syntax-error "There are no variants" type-name type-name?))
    ((define-datatype type-name type-name?
       (variant-name (field-name field-name?) ...)
       ...)
     (begin
       (define type-name (cons '(variant-name ...) '((variant-name field-name ...) ...)))
       (define (type-name? variant)
	 (and (pair? variant)
	      (memq (car variant) (car (car type-name)))
	      #true))
       (set! type-name (cons type-name type-name?))
       (define variant-name
	 (let ((field-names '(field-name ...))
	       (numfields   (length '(field-name ...)))
	       (pred-names '(field-name? ...))
	       (preds      (list field-name? ...)))
	   (lambda args
	     (unless (= (length args) numfields)
	       (error "Expected different number of arguments" numfields args))
	     (for-each (lambda (a f p pname)
			 (unless (p a)
			   (error "field doesn't satisfy predicate" (cons f a) pname)))
		       args field-names preds pred-names)
	     (cons 'variant-name args))))
       ...))))

;;;         tree cases special form example         ;;;
;; (cases tree t                                     ;;
;;   ((leaf w)     (display t)                       ;;
;; 	           (display " is a leaf tree\n"))    ;;
;;   ((node w l r) (display t)                       ;;
;;                 (display " is a node tree\n")))   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax cases
  (syntax-rules ()
    ((cases type-name expr . clauses)
     (let ((type-pred? (cdr type-name)) (x expr))
       (if (type-pred? x)
	   (case-helper x . clauses)
	   (error "expression doesn't satisfy predicate" (cons 'type-name x) type-pred?))))))

(define-syntax case-helper
  (syntax-rules (else)
    ((case-helper variant (else body0 body1 ...))
     (begin body0 body1 ...))
    ((case-helper variant ((variant-name field-name ...) body0 body1 ...))
     (if (eq? (car variant) 'variant-name)
	 (apply (lambda (field-name ...) body0 body1 ...)
		(cdr variant))
	 (error "variant isn't handled in cases form" (car variant))))
    ((case-helper variant ((variant-name field-name ...) body0 body1 ...) clause ...)
     (if (eq? (car variant) 'variant-name)
	 (apply (lambda (field-name ...) body0 body1 ...)
		(cdr variant))
	 (case-helper variant clause ...)))
    ((case-helper other ...)
     (syntax-error "malformed case form" other ...))))
