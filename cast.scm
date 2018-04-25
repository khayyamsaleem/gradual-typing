#|
	Intermediate Typed Language
|#

(load "pmatch.scm")
(load "types.scm")

(define call/cc call-with-current-continuation)

(define-syntax fn-erase
  (syntax-rules (:)
    ((_ (: v type))
     '(v))
    ((_ ((: v type) v2 ...))
     `(v ,@(fn-erase v2 ...)))))

(define-syntax fn
  ;; Can we avoid the use of eval here?
  (syntax-rules (:)
    ((_ (: v type) (: return) body ...)
     (lambda (v) body ...))
    ((_ (: v type) body ...)
     (lambda (v) body ...))
    ((_ ((: v type) v2 ...) (: return) body ...)
     (fn ((: v type) v2 ...) body ...))
    ((_ ((: v type) v2 ...) body ...)
     (let ((env (the-environment)))
       (eval `(lambda ,(fn-erase ((: v type) v2 ...)) body ...) env)))
    ((_ () body ...)
     (lambda () body ...))))

(define (cast-symbol e type)
  `(: ,e ,type))

(define (cast? expr)
  (and (pair? expr) (eq? (car expr) ':)))

(define (type-of-cast expr)
  (caddr expr))

(define (expr-of-cast expr)
  (cadr expr))

(define (make-cast expr type)
  `(: ,expr ,type))

(define (uncast cast)
  (and (eq? (car cast) ':) (cadr cast)))

;; Cast so far cannot handle multiple arguments in lambda, or application
;; Need better error checking
;; Cases to cast:
;; Unary Functions - (func)
;; N-ary Functions - (func x y z ...)
;; Unary Lambda    - (fn () ...)
;; N-ary Lambda    - (fn ((: x number) (: y number)) (+ x y))
(define (cast expr te)
  (define (make-cast expr type)
    `(: ,expr ,type))
  (define (uncast cast)
    (and (eq? (car cast) ':) (cadr cast)))
  (call/cc 				; REMOVE
   (lambda (break)
     (pmatch
      expr
      (,e (guard (cast? e)) e)
      ;; ((: ,e ,t) (make-cast (cast e te) t))
   
      (,e (guard (symbol? e))
	  (let ((typed? (te/lookup te e)))
	    (if typed? (make-cast e typed?) (make-cast e 'any))))

      ((lambda (,v) ,body)
       (cast `(fn (: ,v any) ,body) te))
      
      ((fn (: ,v ,type) ,body)
       (let* ((body-cast (cast body (cons (cons v type) te)))
	      (co-domain-type (type-of-cast body-cast)))
	 (make-cast `(fn (: ,v ,type) ,body-cast) (make-arrow-type type co-domain-type))))
   
      ((fn (: ,v ,type) (: ,ret-type) ,body)
       (let* ((body-cast (cast body (cons (cons v type) te)))
	      (co-domain-type (type-of-cast body-cast)))
	 (if (~ co-domain-type ret-type)
	     (make-cast `(fn (: ,v ,type) ,(make-cast (uncast body-cast) ret-type)) (make-arrow-type type ret-type))
	     ;; (break 'cast-error)
	     (error "CastError")
	     )))

      ((fn () ,body)
       (let* ((body-cast (cast body te))
	      (co-domain-type (type-of-cast body-cast)))
	 (make-cast `(fn () ,body-cast) (make-arrow-type 'unit co-domain-type))))

      ((,rator ,rand)
       (let* ((rator-cast (cast rator te))
	      (rator-type (type-of-cast rator-cast))
	      (rand-cast (cast rand te))
	      (rand-type (type-of-cast rand-cast)))
	 (if (any-type? rator-type)
	     (make-cast (list (make-cast (uncast rator-cast) (make-arrow-type rand-type 'any))
			      rand-cast) 'any)
	     (if (equal? (domain rator-type) rand-type)
		 (make-cast (list rator-cast rand-cast) (co-domain rator-type))
		 (if (~ (domain rator-type) rand-type)
		     (make-cast (list rator-cast
				      (make-cast (uncast rand-cast) (domain rator-type)))
				(co-domain rator-type))
		     ;; (break 'cast-error)
		     (error "CastError"))))))

      (,e (guard (number? e)) (make-cast e 'number))
      (,e (guard (string? e)) (make-cast e 'string))
      (,e (guard (boolean? e)) (make-cast e 'boolean))
      ;; doesn't let the else expression get evaluated
      (,e (make-cast e (car (dispatch-tag-contents (dispatch-tag e)))))
      (else (break 'not-implemented))))))


(define (tj expr type te)
  (let* ((cast-expr (cast expr te))
	 (cast-type (type-of-cast cast-expr)))
    (if (and (~ type cast-type) (not (equal? type cast-type)))
	(let ((new-cast (make-cast (uncast cast-expr) type)))
	  (begin (display "TCast - ") (display cast-type) (display " => ")
		 (display type) (display " :- ")
		 (display new-cast) (newline)
		 (tj (make-cast (uncast cast-expr) type) type te)))
	(pmatch
	 cast-expr
	 ((: ,e ,s) (guard (symbol? e))
	  (if (equal? type s)
	      ;; Check if the variable is already in the environment
	      (cons (cons e s) te)
	      (error "Unexpected...")))
	 ((: (fn (: ,v ,s) ,body) ,t)
	  (if (equal? t type)
	      (begin (tj body (co-domain t) (cons (cons v s) te))
	      	     te)
	      ;; (tj body (co-domain t) (cons (cons v s) te))
	      (error "Function Cast -- Unexpected ...")))
	 ((: (,rator ,rand) ,t)
	  ;; Need to do
	  (display `(: (,rator ,rand) ,t)))
	 (else (error "Not implemented"))))))

(define (in-env? expr env te)
  (or (environment-bound? env expr)
      (assoc expr te)))

(define (tck expr te)
  (let ((env (ge (nearest-repl/environment))) ;; need to fix this...
	(cast-expr (cast expr te)))
    (if (eq? cast-expr 'cast-error)
	(error "Type checking failed with a CastError")
	(pmatch
	 cast-expr
	 ((: ,e ,s) (guard (symbol? e))
	  (let ((binding (in-env? e env te)))
	    (if (not binding)
		(error "Undefined variable --" e)
		te)))
	 ((: (fn (: ,v ,s) ,body) ,t)
	  (let* ((tck-body (tck body (cons (cons v s) te)))
		 (body-type (type-of-cast body)))
	    (if (equal? body-type (co-domain t))
		te
		(error "Something went wrong"))))
	 ((: (fn (: ,v ,s) (: ,ret) ,body) ,t)
	  (let* ((tck-body (tck body (cons (cons v s) te)))
		 (body-type (type-of-cast body)))
	    (if (equal? body-type (co-domain t))
		te
		(error "Something went wrong ..."))))
	 ((: (,rator ,rand) ,type)
	  (let* ((tck-rator (tck rator te))
		 (tck-rand (tck rand te)))
	    te))
	 (else (error "Not implemented yet"))))))

(define (tco expr te)
  (define (j te ty) (cons te ty))
  (define (j-te obj) (car obj))
  (define (j-ty obj) (cdr obj))
  (let ((cexpr (cast expr te)))
    (if (eq? cexpr 'cast-error) (error "Type Checking failed with CastError")
	(pmatch
	 cexpr
	 ((: ,e ,s) (guard (symbol? e))
	  (let ((binding (te/lookup te e)))
	    (if binding
		(if (and (~ s binding) (not (equal? s binding)))
		    (begin (display "Bound type of") (display e) (display " : ") (display binding)
			   (newline)
			   (display "Type Given : ") (display s) (newline)
			   (display "Switching due to ~") (newline)
			   ;; (error "Inconsistent types")
			   (tco `(: ,e ,binding) te))
		    (j te binding))
		(j (cons (cons e s) te) s))))
	 ((: (,rator ,rand) ,t)
	  (let* ((rator-check (tco rator te))
		 (rand-check (tco rand te))
		 (rator-type (j-ty rator-check))
		 (rand-type (j-ty rand-check)))
	    (begin (display "Rator type: ") (display rator-type) (newline)
		   (display "Rand type: ") (display rand-type) (newline)
		   (if (arrow-type? rator-type)
		       (if (~ (domain rator-type) rand-type)
			   (if (~ (co-domain rator-type) t)
			       (j (j-te rator-check) rand-type)
			       (error "Inconsistent co-domain type"))
			   (error "Inconsistent domain type"))
		       (error "Not a function --" rator)))))
	 (else 'not-implemented)))))

(define (tcc expr te)
  (let ((cexpr (cast expr te)))
    (if (eq? cexpr 'cast-error)
	(error "Type Checking failed with CastError")
	(pmatch
	 cexpr
	 ;; We are performing too many checks here, we might
	 ;; not have to
	 ((: ,e ,type) (guard (symbol? e))
	  (let ((binding (te/lookup te e)))
	    (if binding
		(if (~ binding type)
		    binding
		    (error "Inconsistency error"))
		type)))
	 ((: (fn (: ,v ,s) ,body) ,t)
	  (let* ((body-type (tcc body (cons (cons v s) te)))
		 (fn-type (make-arrow-type s body-type)))
	    (if (~ fn-type t)
		fn-type
		(error "Inconsistent types"))))
	 ((: (fn (: ,v ,s) (: ,ret) ,body) ,t)
	  (let* ((body-type (tcc body (cons (cons v s) te))))
	    (if (~ body-type ret)
		(let ((fn-type (make-arrow-type s ret)))
		  (if (~ fn-type t)
		      fn-type
		      (error "Inconsistent types")))
		(error "Inconsistent types"))))
	 ((: (,rator ,rand) ,t)
	  (let* ((rator-type (tcc rator te))
		 (rand-type (tcc rand te)))
	    (begin (display "Rator type : ") (display rator-type)
		   (newline)
		   (display "Rand type : ") (display rand-type)
		   (newline)
		   (if (arrow-type? rator-type)
		       (if (~ (domain rator-type) rand-type)
			   (if (~ (co-domain rator-type) t)
			       t
			       (error "Inconsistent types"))
			   (error "Inconsistent types"))
		       (error "Cannot apply non function")))))
	 ((: ,e number) (guard (number? e)) 'number)
	 ((: ,e boolean) (guard (boolean? e)) 'boolean)
	 ((: ,e string) (guard (string? e)) 'string)
	 ((: ,e char) (guard (char? e)) 'char)
	 (else 'not-implemented)))))


(define (te/ext te var type)
  (let ((binding (assoc var te)))
    (if binding
	(begin (set-cdr! binding type) te)
	(cons (cons var type) te))))

(define (te/look te var)
  (let ((binding (assoc var te)))
    (if binding
	(cdr binding)
	#f)))

(define (te/join te1 te2)
  (if (null? te1) te2
      (let ((binding (assoc (caar te1) te2)))
	(if binding
	    (if (~ (cdr binding) (cdar te1))
		(te/join (cdr te1) te2)
		(error "Inconsistent types in Environments"))
	    (cons (car te1) (te/join (cdr te1) te2))))))

(define (te/del te var)
  (del-assoc var te))

;; (define (type-check expr te)
;;   (let ((cexpr (cast expr te)))
;;     (pmatch
;;      cexpr
;;      ((: ,e ,type) (guard (symbol? e))
;;       (let ((binding (te/look te e)))
;; 	(if binding
;; 	    (if (~ binding type)
;; 		te
;; 		(error "Inconsistent types" 'expected type 'got binding 'for e))
;; 	    (te/ext te e type))))
;;      ((: (,rator ,rand) ,type)
;;       ;; need to change this?
;;       (let* ((tc-rator (type-check rator te))
;; 	     (tc-rand (type-check rand te)))
;; 	(if (~ (co-domain (type-of-cast rator)) type)
;; 	    (if (~ (domain (type-of-cast rator)) (type-of-cast rand))
;; 		(te/join tc-rand tc-rator)
;; 		(error "Inconsistent types" 'expected (domain (type-of-cast rator))
;; 		       'got (type-of-cast rand) 'for (expr-of-cast rand)))
;; 	    (error "Inconsistent types" 'expected type 'got (type-of-cast rator) 'for
;; 		   (expr-of-cast rator)))))
;;      (else 'not-implemented))))

(define (type-check expr te)
  (define (tc exp type te)
    (pmatch
     exp
     ((: ,e ,t) (guard (and (~ type t) (not (equal? type t)))) ; TCAST
      (begin (display e) (display " : Cast => ") (display t) (display " ") (display type) (newline))
      (tc `(: ,e ,type) type te))
     ((: ,e ,t) (guard (and (symbol? e))) ; TVAR
      (let ((binding (te/look te e)))
	(if binding
	    (begin (display e) (display " : Binding => ") (display binding) (newline)
		   (if (~ binding type)
		       (te/ext te e type)
		       (error "Inconsistent types" 'exptected type 'got binding 'for e)))
	    (begin (display e) (display " : NotBound => ") (display type) (newline)
		   (te/ext te e type)))))
     ((: (,rator ,rand) ,t) ;; (guard (equal? type t))
      (let* ((tc-rator (tc rator `(-> ,(type-of-cast rand) ,type) te))
	     (tc-rand (tc rand (domain (type-of-cast rator)) te)))
	(te/join tc-rator tc-rand)))
     ((: (fn (: ,v ,s) ,body) ,t)
      (let* ((tc-body (tc body (co-domain type) (te/ext te v s))))
	(te/del tc-body v)))
     ((: (fn (: ,v ,s) (: ,ret) ,body) ,t)
      (let* ((tc-body (tc body ret (te/ext te v s))))
	(te/del tc-body v)))
     ((: ,e number) (guard (number? e)) te)
     ((: ,e string) (guard (string? e)) te)
     ((: ,e boolean) (guard (boolean? e)) te)
     ((: ,e char) (guard (char? e)) te)
     (else 'not-implemented)))
  (let ((c (cast expr te)))
    (tc c (type-of-cast c) te)))

(define (tc1 exp type te)
    (pmatch
     (cast exp te)
     ((: ,e ,t) (guard (and (~ type t) (not (equal? type t)))) ; TCAST
      (begin (display e) (display " : Cast => ") (display t) (display " ") (display type) (newline))
      (tc1 `(: ,e ,type) type te))
     ((: ,e ,t) (guard (and (symbol? e))) ; TVAR
      (let ((binding (te/look te e)))
	(if binding
	    (begin (display e) (display " : Binding => ") (display binding) (newline)
		   (if (~ binding type)
		       (te/ext te e type)
		       (error "Inconsistent types" 'exptected type 'got binding 'for e)))
	    (begin (display e) (display " : NotBound => ") (display type) (newline)
		   (te/ext te e type)))))
     ((: (,rator ,rand) ,t) ;; (guard (equal? type t))
      (let* ((tc-rator (tc1 rator `(-> ,(type-of-cast rand) ,type) te))
	     (tc-rand (tc1 rand (domain (type-of-cast rator)) te)))
	(te/join tc-rator tc-rand)))
     ((: (fn (: ,v ,s) ,body) ,t)
      (let* ((tc-body (tc1 body (co-domain type) (te/ext te v s))))
	(te/del tc-body v)))
     ((: (fn (: ,v ,s) (: ,ret) ,body) ,t)
      (let* ((tc-body (tc1 body ret (te/ext te v s))))
	(te/del tc-body v)))
     ((: ,e number) (guard (number? e)) te)
     ((: ,e string) (guard (string? e)) te)
     ((: ,e boolean) (guard (boolean? e)) te)
     ((: ,e char) (guard (char? e)) te)
     (else 'not-implemented)))

(define
  example-1
  '(: (fn (: f (-> number string))
	  (: (fn (: v number)
		 (: ((: f (-> number string))
		     (: v number))
		    string))
	     (-> number string)))
      (-> (-> number string) (-> number string))))
