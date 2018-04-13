(load "linear-match.scm") ;; pattern matcher

#|
    no-eval: quotes expression
|#
(define-syntax no-eval
  (syntax-rules ()
    ((_ expr)
     'expr)))

#|
    An interface for type expressions

    Ground types
     . number, fixnum, flonum, bignum, recnum (complex numbers)
     . boolean
     . string, character

    Function types
     . (-> s t)
|#
(define ground-types
  '(number fixnum flonum bignum boolean))


(define (ground-type? type)
  (member type ground-types))

(define (make-arrow s t)
  `(-> ,s ,t))

(define (arrow-type? type)
  (and (pair? type) (equal? (car type) '->)))

(define (domain arrow-type)
  (second arrow-type))

(define (codomain arrow-type)
  (third arrow-type))

(define (any-type? type)
  (equal? type 'any))


#|
    Type consistency operator (~)
    - Reflexive
    - Symmetric
    - NOT Transitive
|#
(define (~ type1 type2)
  (cond ((or (any-type? type1) (any-type? type2)) #t)
	((equal? type1 type2) #t)
	((and (pair? type1) (pair? type2))
	 (and (~ (car type1) (car type2))
	      (~ (cdr type1) (cdr type2))))
	(else #f)))

#|
    Before we start type checking we need to define the
    syntax of our extensions to Scheme.
|#
(define-syntax typed-lambda
  (syntax-rules (:)
    ((_ (: var type) body ...)
     ;; does nothing special for now. At a later stage
     ;; we will want to type check the expression here.
     ;; e.g. (typed-lambda (: x number) x) has to type
     ;;      check with (-> number any)
     (lambda (var) body ...))))

(define-syntax :
  (syntax-rules ()
    ((: val type)
     ;; does nothing now. Will eventually tell the type
     ;; checker that the value has a certain type.
     ;; Usage: (: 17 number) (: '(1 2 3) list)
     ;;        (: (lambda (x) (+ x 1)) (-> number number))
     val)))


(define (type-check expr type gamma)
  (match expr
    ;; For now it seems hard to not evaluate certain
    ;; objects such as numbers and booleans. This is
    ;; because '3 => 3 and '#t => #t. Same for strings.
    (,x (any-type? type) type)

    (,x (symbol? x) (if (assoc x gamma)
			(let ((bound-type (cdr (assoc x gamma))))
			  (if (equal? type bound-type)
			      (cdr (assoc x gamma))
			      (error "Type checking faied -- " expr 'with type)))
			type))

    (,x (and (number? x) (~ type 'number)) type)

    (,x (and (boolean? x) (~ type 'boolean)) type)

    (,x (assoc x gamma) (cdr (assoc x gamma)))

    ;; We should not be doing this. Need to resolve
    ;; types of internal procedures in some other manner
    (('+ ,x . ,y) (~ type 'number) type)
    (('- ,x . ,y) (~ type 'number) type)
    (('* ,x . ,y) (~ type 'number) type)
    (('/ ,x . ,y) (~ type 'number) type)

    (('typed-lambda (: ,x ,s) ,body) (arrow-type? type)
     (if (not (~ s (domain type)))
	 (error "Inconsistent parameter type -- " s 'with (domain type))
	 (let ((param-type s))
	   (make-arrow param-type
		       (type-check body (codomain type) (cons (cons x param-type) gamma))))))
    
    (__ () (error "Type checking failed -- " expr 'with type))))


;; Examples
(type-check (no-eval (+ 1 2)) 'number '()) ; => 'number
;; (type-check (no-eval (+ 1 2)) 'boolean '()) ; => ERROR

(type-check (no-eval (typed-lambda (: x number) (+ x 1)))
	    '(-> number any) '()) ; => (-> number any)

(type-check (no-eval (typed-lambda (: x number) x))
	    '(-> number number) '()) ; => (-> number number)

;; (type-check (no-eval (typed-lambda (: x string) (+ x 1)))
;; 	    '(-> number string) '()) ; => ERROR

(type-check (no-eval (typed-lambda (: x number)
				   (typed-lambda (: y string)
						 y)))
	    '(-> number (-> string string)) '())
