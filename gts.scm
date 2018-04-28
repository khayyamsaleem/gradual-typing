#|
	Poor Man's Gradual Type System v.0.1
	Khayyam Saleem, Ramana Nagasamudram
|#

(load "pmatch.scm")
(load "types.scm")

#|
	Syntactic Extensions
	- terms with type annotations
|#

(define-syntax typed-lambda
  (syntax-rules (::)
    ((_ (:: p type) body ...)
     (lambda (p) body ...))
    ((_ (p :: type) body ...)
     (lambda (p) body ...))))

(define-syntax fn
  (syntax-rules (::)
    ((_ (:: p type) body)
     (let ((tchecked (tc '(typed-lambda (:: p type) body)
			 '(-> type any)
			 '())))
       (begin (display tchecked) (lambda (p) body))))))

(define (typed-lambda? expr)
  (equal? (car expr) 'typed-lambda))

#|
	Types
	. any, boolean, number, symbol, string, character
	. pair, list
|#

;; ;;;; Any Type
;; (define (any-type? type)
;;   (eq? type 'any))

;;;; Arrow (Function) Types
;; (define (make-arrow-type s t)
;;   `(-> ,s ,t))

;; (define (arrow-type? type)
;;   (and (pair? type) (eq? (car type) '->)))

;; (define (domain arrow-type)
;;   ;; (if (eq? arrow-type 'any)
;;   ;;     'any
;;   ;;     (cadr arrow-type))
;;   (cadr arrow-type))

;; (define (co-domain arrow-type)
;;   ;; (if (eq? arrow-type 'any)
;;   ;;     'any
;;   ;;     (caddr arrow-type))
;;   (caddr arrow-type))

;;;; Pairs, Lists, Hash-tables
;; TODO

;;;; Types for predefined functions
;; (define (application-type symbol)
;;   (let ((number-ops '(+ - * /)))
;;     (if (member symbol number-ops)
;; 	'(-> number number)
;; 	'(-> any any))))

;;;; Type consistency
;; (define (~ s t)
;;   (cond ((or (any-type? s) (any-type? t)))
;; 	((equal? s t))
;; 	((and (pair? s) (pair? t))
;; 	 (and (~ (car s) (car t))
;; 	      (~ (cdr s) (cdr t))))
;; 	(else #f)))

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
  (if (not (te/lookup te var))
      (cons (cons var type) te)
      te))

(define (te/lookup te var)
  (let ((type (assoc var te)))
    (if type (cdr type) #f)))

;;;; Type checker
;; Doesn't handle functions that take multiple arguments such as +,-,*,/
;; Arity needs to be handled
(define (tc expr type te)
  (pmatch
   expr

   ((typed-lambda (:: ,v ,s) ,body)
    (if (~ s (domain type))
	(let ((te2 (tc body (co-domain type) (te/extend te v s))))
	  (te/extend te2 expr type))
	(tc-error expr s (domain type))))

   ;;     Γ, x:s |- M : t
   ;; -------------------------
   ;;   Γ |- λx:s.M : s -> t
   ;; ((typed-lambda (:: ,v ,s) ,body)
   ;;  (if (~ s (domain type))
   ;; 	(tc body (co-domain type) (te/extend te v s))
   ;; 	(tc-error expr s (domain type))))

   ((if ,pred ,t-clause ,f-clause)
    (let* ((pred-type (tc pred 'boolean te))
	   (t-type (tc t-clause type pred-type))
	   (f-type (tc f-clause type pred-type)))
      (if (~ t-type f-type)
	  te
	  (tc-error expr t-type f-type))))

   ;; Γ |- M : s -> t  Γ |- N : s
   ;; ---------------------------
   ;;        Γ |- M N : t
   ((,rator . ,rands) (guard (predefined? rator))
    (let* ((rator-type (get-predefined-type rator))
	   (rator-domain (domain rator-type))
	   (ar (arity rator-type)))
      (if (or (equal? ar 'n) (equal? (length rands) ar))
	  (if (~ type (co-domain rator-type))
	      (fold-left (lambda (acc r) (tc r rator-domain acc)) te rands)
	      (tc-error expr type (co-domain rator-type)))
	  (error "Incorrect number of arguments to" 'rator 'expected ar 'got (length rands)))))

   ;; (((typed-lambda (:: ,e ,t) ,body) . ,rands)
   ;;  (display `(typed-lambda (:: ,e ,t) ,body)) (newline) (display rands) (newline))

   ;; operator applied to a number of operands
   ((,rator . ,rands)
    ;; first we need to check if rator is in te
    (let ((rator-type (te/lookup te rator)))
      (if rator-type
	  (let ((rator-domain (domain rator-type)))
	    (if (~ (co-domain rator-type) type)
		(fold-left (lambda (acc r) (tc r rator-domain acc)) te rands)
		(tc-error expr type (co-domain rator-type))))
	  (let ((te2 (tc rator (make-arrow-type 'any type) te)))
	    (fold-left (lambda (acc r) (tc r 'any acc)) te2 rands)))))

   ;; ((,rator ,rand)
   ;;  ;; first we need to check if rator is in te
   ;;  (let ((rator-type (te/lookup te rator)))
   ;;    (if rator-type
   ;; 	  ;; (tc rand (domain rator-type) (te/extend te rator rator-type))
   ;; 	  (if (~ type (co-domain rator-type))
   ;; 	      (tc rand (domain rator-type) te))
   ;; 	  (tc rand (domain (application-type rator))
   ;; 	      ;; (te/extend te rator (application-type rator))
   ;; 	      ;; a 'hack'
   ;; 	      (te/extend te rator (make-arrow-type 'any type))))))

   ;; (,e (guard (symbol? e))
   ;;     (begin (display "make sure symbol-case works properly") (newline)
   ;; 	      (let ((predef (get-predefined-type e))
   ;; 		    (bound-type (te/lookup te e)))
   ;; 		(begin (display predef) (newline) (display bound-type) (newline)
   ;; 		       (cond ((and predef bound-type)
   ;; 			      (if (~ predef bound-type)
   ;; 				  (if (~ bound-type type)
   ;; 				      te
   ;; 				      (tc-error e type bound-type))
   ;; 				  (tc-error e predef bound-type)))
   ;; 			     (predef
   ;; 			      (if (~ predef type)
   ;; 				  te
   ;; 				  (tc-error expr type predef)))
   ;; 			     (bound-type
   ;; 			      (if (~ bound-type type) te (tc-error expr type bound-type)))
   ;; 			     (else (te/extend te e type)))))))

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


#|
	Examples
|#

;; Works
;; (tc '(f x) 'any '())

;; Works
;; (tc '(typed-lambda (:: f (-> any any))
;; 		   (typed-lambda (:: x any)
;; 				 (f x)))
;;     '(-> (-> any any) (-> any any)) '())

;; f's type is (-> any any). Need to fix so that it is
;; (-> number any)
;; (tc '(typed-lambda (:: x number) (f x))
;;     '(-> number any) '())

;; ERROR : object passed as an argument to safe-cdr is not a pair.
;; Need to fix error message but this is the behavior we want.
;; (tc '(typed-lambda (:: f any) (f f)) 'any '())
