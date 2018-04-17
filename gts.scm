#|
	Poor Man's Gradual Type System v.0.1
	Khayyam Saleem, Ramana Nagasamudram
|#

(load "pmatch.scm")

#|
	Syntactic Extensions
	- terms with type annotations
|#

(define-syntax typed-lambda
  (syntax-rules (::)
    ((_ (:: p type) body ...)
     (lambda (p) body ...))))

(define (typed-lambda? expr)
  (equal? (car expr) 'typed-lambda))

#|
	Types
	. any, boolean, number, symbol, string, character
	. pair, list
|#

;;;; Any Type
(define (any-type? type)
  (eq? type 'any))

;;;; Arrow (Function) Types
(define (make-arrow-type s t)
  `(-> ,s ,t))

(define (arrow-type? type)
  (and (pair? type) (eq? (car type) '->)))

(define (domain arrow-type)
  (cadr arrow-type))

(define (co-domain arrow-type)
  (caddr arrow-type))

;;;; Pairs, Lists, Hash-tables
;; TODO

;;;; Types for predefined functions
(define (application-type symbol)
  (let ((number-ops '(+ - * /)))
    (if (member symbol number-ops)
	'(-> number number)
	'(-> any any))))

;;;; Type consistency
(define (~ s t)
  (cond ((or (any-type? s) (any-type? t)))
	((equal? s t))
	((and (pair? s) (pair? t))
	 (and (~ (car s) (car t))
	      (~ (cdr s) (cdr t))))
	(else #f)))

#|
	Type Checker

	TODO
	. Handle functions that take in multiple arguments, especially +,-,*,/
	. Clean up application case
	. Type check if, cond, let, let*, letrec
|#

(define (tc-error expr s t)
  (error "Type checking failed -- " s 'with t '-- expr))

;;;; Type environments -- association lists (for now)
(define (te/extend te var type)
  (cons (cons var type) te))

(define (te/lookup te var)
  (let ((type (assoc var te)))
    (if type (cdr type) #f)))

;;;; Type checker
;; Doesn't handle functions that take multiple arguments such as +,-,*,/
(define (tc expr type te)
  (pmatch
   expr
   ((typed-lambda (:: ,v ,s) ,body)
    (if (~ s (domain type))
	(tc body (co-domain type) (te/extend te v s))
	(tc-error expr s (domain type))))
   ((,rator ,rand)
    ;; first we need to check if rator is in te
    (let ((rator-type (te/lookup te rator)))
      (begin (display rator-type) (newline)
	     (if rator-type
		 (tc rand (domain rator-type) (te/extend te rator rator-type))
		 (tc rand (domain (application-type rator))
		     (te/extend te rator (application-type rator)))))))
   (,e (guard (symbol? e))
       (let ((bound-type (te/lookup te e)))
	 (if bound-type
	     (if (~ bound-type type) te (tc-error expr type bound-type))
	     (te/extend te expr type))))
   (,e (guard (number? e)) (if (~ type 'number) te (tc-error expr type 'number)))
   (,e (guard (boolean? e)) (if (~ type 'boolean) te (tc-error expr type 'boolean)))
   (,e (guard (string? e)) (if (~ type 'string) te (tc-error expr type 'string)))
   (,e (guard (char? e)) (if (~ type 'char) te (tc-error expr type 'char)))
   (else (tc-error expr type #f))))
