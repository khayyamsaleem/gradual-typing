#|
	Type Environments
|#

(load "linear-match.scm")

(define (make-tenv) '())

(define (extend tenv var val)
  (cons (cons var val) tenv))

(define (get-type tenv var)
  (let ((bound-value (assoc var tenv)))
    (if bound-value
	(cdr bound-value)
	#f)))

#|
	Types and Type consistency
|#

(define (any-type? type)
  (eq? type 'any))

(define (arrow-type? type)
  (and (pair? type) (equal? (car type) '->)))

(define (domain arrow-type)
  (second arrow-type))

(define (codomain arrow-type)
  (third arrow-type))

(define (make-arrow s t)
  `(-> ,s ,t))

(define (~ type1 type2)
  (cond ((or (any-type? type1) (any-type? type2)) #t)
	((equal? type1 type2) #t)
	((and (pair? type1) (pair? type2))
	 (and (~ (car type1) (car type2))
	      (~ (cdr type1) (cdr type2))))
	(else #f)))

#|
	Syntactic Extensions
|#

(define-syntax typed-lambda
  (syntax-rules (:)
    ((_ (: x type) body ...)
     (lambda (x) body ...))))

(define (param-type expr)
  (caddr expr))

#|
	Type Checking
|#

(define-syntax no-eval
  (syntax-rules ()
    ((_ expr)
     (quote expr))))

(define (predefined-type symbol)
  (let ((number-ops '(+ - * /)))
    (if (member symbol number-ops)
	'(-> number number)
	'(-> any any))))

(define (typed-lambda? expr)
  (equal? (car expr) 'typed-lambda))

(define application? pair?)

(define (if-expr? expr)
  (equal? (car expr) 'if))

(define if-predicate cadr)
(define if-true caddr)
(define if-false cadddr)


#|
	Type-Check needs to return Γ, not the type
|#
(define (type-check expr type gamma)
  (cond
   ((number? expr) (if (~ type 'number)
		       type
		       (error "Type checking failed --" type
			      'with 'number 'for expr)))

   ((boolean? expr) (if (~ type 'boolean)
			type
			(error "Type checking failed --" type
			       'with 'boolean 'for expr)))

   ((symbol? expr)
    (begin (display "symbol branch -- ") (display expr)
	   (display ":") (display type) (newline)
	   (let ((bound-type (get-type gamma expr)))
	     (if bound-type
		 (if (~ bound-type type)
		     (begin (display type)
			    (newline)
			    type)
		     (error "Expected" bound-type 'got type 'for expr))
		 type))))

	
   ((typed-lambda? expr)		; lambda
    (type-check-lambda expr type gamma))

   ((if-expr? expr)
    (begin (display "if branch -- ") (display expr) (display ":")
	   (display type) (newline)
	   (display "doesn't work... need to make sure gamma is consistent")
	   (let ((tc-predicate (type-check (if-predicate expr)
					   'boolean gamma))
		 (tc-if-true (type-check (if-true expr) type gamma))
		 (tc-if-false (type-check (if-false expr) type gamma)))
	     (if (~ tc-if-true tc-if-false)
		 tc-if-true
		 (error "If branch types don't match --" tc-if-true 'and tc-if-false
			'for expr)))))

   
   ((pair? expr)
    (begin (display "application branch -- ") (display expr)
	   (display ":") (display type) (newline)
	   (display "gamma: ") (display gamma) (newline)
	   (let* ((rator (car expr))
		  (rand (cdr expr))
		  (predefined (predefined-type rator)))
	     (begin (display "need to fix...") (newline)
		    (car (map (lambda (x)
				(type-check x (domain predefined)
					    gamma)) rand))))))
	
   (else "Don't know how to typecheck")))

#|
	   Γ,x:σ ⊦ M : τ
	------------------
	Γ ⊦ λx:σ.M : σ → τ
|#

(define (type-check-lambda expr type gamma)
  ;; Expects a `typed-lambda' expression
  (let* ((param (cadr expr))
	 (body (caddr expr))
	 (p-type (param-type param)))
    (if (~ p-type (domain type))
	(make-arrow p-type (type-check body (codomain type)
				       (extend gamma (cadr param) (domain type))))
	(error "Inconsistent types --" p-type 'with type 'for expr))))

