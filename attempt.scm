;;;; TODO 
;;; pattern matcher -> list of pairs (<expr> . <type>)
;;; match symbols against whats in the env.
;;; no-eval : tokenize
;;; get-type without eval
;;; type consistency checker
;;; typed lambda

(define type-environment (make-hash-table))

(define (get-type val)
  (cond ((number? val) 'number)
	((list? val) 'list)
	((pair? val) 'pair)
	((string? val) 'string)
	((procedure? val) 'procedure)
	((symbol? val) 'symbol)
	((char? val) 'char)
	((boolean? val) 'boolean)
	(else (hash-table/get type-environment val 'any))))

(define-syntax defvar
  (syntax-rules (::)
    ((_ (:: name type) val)
     (if (equal? (get-type val) type)
	 (begin (hash-table/put! type-environment 'name type)
		(environment-define (the-environment) 'name val))
	 (error "Inconsistent types -- " type 'and (get-type val))))))

(define-syntax setvar
  (syntax-rules ()
    ((_ name val)
     (let ((type (hash-table/get type-environment 'name #f))
	   (env (the-environment)))
       (if (equal? type (get-type val))
	   (environment-assign! env 'name val)
	   (error "Inconsistent types -- " type 'and (get-type val)))))))

(define-syntax tlambda
  (syntax-rules (::)
    ((_ (:: var var-type) body ...)
     (begin
       (hash-table/put! type-environment (lambda (var) body ...) (-> var-type (generate-uninterned-symbol)))
       (lambda (var) body ...)))))

(define-syntax symbol-procedure?
  (syntax-rules ()
    ((_ symbol)
     (eval `(procedure? ,symbol) (the-environment)))))

(define-syntax symbol-compound-procedure?
  (syntax-rules ()
    ((_ symbol)
     (eval `(compound-procedure? ,symbol) (the-environment)))))

(define (typer expr)
  (dispatch-tag expr))

(define (typo expr)
  (dispatch-tag-contents (dispatch-tag expr)))

(define-syntax type-symbol
  (syntax-rules ()
    ((_ symbol)
     (let ((env (the-environment)))
       (eval `(typo ,symbol) env)))))

(define (type expr)
  (cond ((list? expr) 'list)
	((pair? expr) 'pair)
	(else (car (typo expr)))))

(define-syntax var
  (syntax-rules (::)
    ((_ (:: name t) val)
     (if (equal? t (type val))
	 (begin
	   (hash-table/put! type-environment 'name (type val))
	   (environment-define (the-environment) 'name val))
	 (error "Expected type " t 'got val)))))

(define-syntax fn
  (syntax-rules (::)
    ((_ (:: arg t) body)
     (begin (display (type body))
	    (if (equal? t (type body))
		(begin (hash-table/put! type-environment 'name `(-> ,t ,(type body)))
		       (lambda (arg) body))
		(error "Expected type " t 'got (type body)))))))

(define-syntax my-apply
  (syntax-rules ()
    ((_ fn ls ...)
     (begin (display "Works")
	    (apply fn ls ...)))))

(define *apply* (environment-lookup system-global-environment 'apply))

(define-syntax apply
  (syntax-rules ()
    ((_ fn ls ...)
     (begin (display "Works")
	    (*apply* fn ls ...)))))

(define *eval* (environment-lookup system-global-environment 'eval))

(define-syntax eval
  (syntax-rules ()
    ((_ expr)
     (let ((env (the-environment)))
       (begin (display "Eval...")
	      (*eval* expr env))))))

(define *extended-scode-eval* (environment-lookup system-global-environment 'extended-scode-eval))

(define-syntax extended-scode-eval
  (syntax-rules ()
    ((_ b1 ...)
     (begin (display "Scode eval works")
	    (*extended-scode-eval* b1 ...)))))


(define *scode-eval* (environment-lookup system-global-environment 'scode-eval))

(define-syntax scode-eval
  (syntax-rules ()
    ((_ b1 ...)
     (begin (display "Scode Eval works")
	    (*scode-eval* b1 ...)))))

(define-syntax no-eval
  (syntax-rules ()
    ((_ expr)
     (quote expr))))
