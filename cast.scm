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

;; Potential bugs in cast
;; (cast '(f (f x y)) '()) Casts fine. Maybe it should cast find and not type check?
;; The real issue here is that we are not passing the type environment along.
;; Should have thought more carefully about the design of this function
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
	     (make-cast `(fn (: ,v ,type) (: ,ret-type)
			     ,(make-cast (uncast body-cast) ret-type))
			(make-arrow-type type ret-type))
	     ;; (break 'cast-error)
	     (error "CastError"))))

      ((fn ((: ,v ,type) . ,res) ,body)
       (let* ( (bindings (map (lambda (x) (cons (expr-of-cast x)
						(type-of-cast x))) res))
	      (body-cast (cast body (append (cons (cons v type) bindings) te)))
	      (co-domain-type (type-of-cast body-cast)))
	 (make-cast `(fn ((: ,v ,type) ,@res) ,body-cast)
		    (make-arrow-type (list->pair-type (map (lambda (x) (type-of-cast x))
							   (cons `(: ,v ,type) res)))
				     co-domain-type))))

      ((fn ((: ,v ,type) . ,res) (: ,ret-type) ,body)
       (let* ((bindings (map (lambda (x) (cons (expr-of-cast x)
					       (type-of-cast x))) res))
	      (body-cast (cast body (append (cons (cons v type) bindings) te)))
	      (co-domain-type (type-of-cast body-cast)))
	 (if (~ co-domain-type ret-type)
	     (make-cast `(fn ((: ,v ,type) (: ,ret-type) ,@res) ,body-cast)
			(make-arrow-type (list->pair-type (map (lambda (x) (type-of-cast x))
							       (cons `(: ,v ,type) res)))
					 ret-type)))))
      ((fn () ,body)
       (let* ((body-cast (cast body te))
	      (co-domain-type (type-of-cast body-cast)))
	 (make-cast `(fn () ,body-cast) (make-arrow-type 'unit co-domain-type))))

      ((fn () (: ,ret) ,body)
       (let* ((body-cast (cast body te))
	      (co-domain-type (type-of-cast body-cast)))
	 (if (~ co-domain-type ret)
	     (make-cast `(fn () (: ,ret) ,body-cast) (make-arrow-type 'unit ret))
	     (error "CastError"))))

      ((,rator ,rand)
       (let* ((rator-cast (cast rator te))
	      (rator-type (type-of-cast rator-cast))
	      (rand-cast (cast rand te))
	      (rand-type (type-of-cast rand-cast)))
	 (if (any-type? rator-type)
	     (make-cast (list (make-cast (uncast rator-cast) (make-arrow-type rand-type 'any))
			      rand-cast) 'any)
	     (if (arrow-type? rator-type)
		 (if (equal? (domain rator-type) rand-type)
		     (make-cast (list rator-cast rand-cast) (co-domain rator-type))
		     (if (~ (domain rator-type) rand-type)
			 (make-cast (list rator-cast
					  (make-cast (uncast rand-cast) (domain rator-type)))
				    (co-domain rator-type))
			 ;; (break 'cast-error)
			 (error "CastError")))
		 (error "CastError")))))

      ((,rator . ,rands)
       (let* ((rator-cast (cast rator te))
	      (rator-type (type-of-cast rator-cast))
	      (rand-cast (map (lambda (e) (cast e te)) rands))
	      (rand-type (list->pair-type (fold-right (lambda (x acc)
							(cons (type-of-cast x)
							      acc)) '() rand-cast))))
	 (if (any-type? rator-type)
	     (make-cast `((: ,rator ,(make-arrow-type rand-type 'any))
			  ,@rand-cast) 'any)
	     (if (arrow-type? rator-type)
		 (if (equal? (domain rator-type) rand-type)
		     `(: (,rator-cast ,@rand-cast) ,(co-domain rator-type))
		     (if (~ (domain rator-type) rand-type)
			 (let ((rc (map (lambda (x y) (make-cast (uncast x) y))
					rand-cast (cdr (domain rator-type)))))
			   `(: (,rator-cast ,@rc) ,(co-domain rator-type)))
			 (error "CastError")))
		 (error "CastError")))))

      ((,rator)
       (let* ((rator-cast (cast rator te))
	      (rator-type (type-of-cast rator-cast)))
	 (if (any-type? rator-type)
	     (make-cast (list (make-cast (uncast rator-cast) (make-arrow-type 'unit 'any))) 'any)
	     (if (arrow-type? rator-type)
		 (make-cast (list rator-cast) (co-domain rator-type))
		 (error "CastError -- Not a function")))))

      (,e (guard (number? e)) (make-cast e 'number))
      (,e (guard (string? e)) (make-cast e 'string))
      (,e (guard (boolean? e)) (make-cast e 'boolean))
      ;; doesn't let the else expression get evaluated
      (,e (make-cast e (car (dispatch-tag-contents (dispatch-tag e)))))
      (else (break 'not-implemented))))))

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

;; te/ext is the root of all evil. Just use (cons (cons v type) te)
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
		       ;; (te/ext te e type)
		       (te/ext te e binding)
		       ;; (cons (cons e binding) te)
		       (error "Inconsistent types" 'exptected type 'got binding 'for e)))
	    (begin (display e) (display " : NotBound => ") (display type) (newline)
		   (te/ext te e type)
		   ;; (cons (cons e type) te)
		   ))))
     ((: (fn (: ,v ,s) ,body) ,t)
      (let* ((tc-body (tc body (co-domain type) ;; (te/ext te v s)
			  (cons (cons v s) te))))
	(te/del tc-body v)
	;; te
	))
     ((: (fn (: ,v ,s) (: ,ret) ,body) ,t)
      (let* ((tc-body (tc body ret ;; (te/ext te v s)
			  (cons (cons v s) te))))
	(te/del tc-body v)
	;; te
	))
     ((: (fn ((: ,v ,s) . ,rest) ,body) ,t)
      (display "*") (newline)
      (let* ((bindings (map (lambda (x) (cons (expr-of-cast x)
					      (type-of-cast x))) rest))
	     (tc-body (tc body (co-domain type) (append
						 (cons (cons v s) bindings) te))))
	(display tc-body) (newline)
	(fold-right (lambda (x acc) (te/del acc (car x))) tc-body
		    (append (cons (cons v s) bindings)))
	;; te
	))
     ((: (fn () ,body) ,t)
      (let* ((tc-body (tc body (co-domain type) te)))
	tc-body))
     ((: (fn () (: ,ret) ,body) ,t)
      (let* ((tc-body (tc body ret te)))
	tc-body))
     ((: (,rator ,rand) ,t) ;; (guard (equal? type t))
      (let* ((tc-rator (tc rator `(-> ,(type-of-cast rand) ,type) te))
	     (tc-rand (tc rand (domain (type-of-cast rator)) te)))
	(te/join tc-rator tc-rand)))
     ((: (,rator . ,rands) ,t)
      (display "*A") (newline)
      (let* ((tc-rator (tc rator (type-of-cast rator) te)) ; this bit is sketchy
	     (tc-rand (fold-right (lambda (x acc) (type-check x acc)) te rands)))
	(te/join tc-rator tc-rand)))
     ((: ,e number) (guard (number? e)) te)
     ((: ,e string) (guard (string? e)) te)
     ((: ,e boolean) (guard (boolean? e)) te)
     ((: ,e char) (guard (char? e)) te)
     (else (begin
	     (display "Not impl.") 
	     (display expr) (newline)
	     (display type) (newline)
	     'not-implemented))))
  (let ((c (cast expr te)))
    (begin (display "Type for tc : ") (display (type-of-cast c)) (newline)
	   (tc c (type-of-cast c) te))))

(define example-1
  '(: (fn (: f (-> number string))
	  (: (fn (: v number)
		 (: ((: f (-> number string))
		     (: v number))
		    string))
	     (-> number string)))
      (-> (-> number string) (-> number string))))

(define example-2
  '(: ((: (fn
	   ((: x number) (: y number))
	   (: x number))
	  (-> (* number number) number))
       (: 3 number)
       (: 4 number))
      number))

(define example-3
  ;; BUG : Returns '((x . number))
  ;; Should return '((x . boolean)) 
  (type-check '((fn (: x number) x) 3) '((x . boolean))))
