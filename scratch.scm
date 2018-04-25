;; Old versions of CAST


;; Has some bugs and doesn't implement the rules accurately
(define (casto expr te)
  (pmatch
   expr
   (,e (guard (symbol? e))
       (let ((typed? (te/lookup te e)))
	 (if typed?
	     `(: ,e ,typed?)
	     `(: ,e any))))
   ((fn (: ,v ,type) ,body)
    ;; Faced an error here with te/extend. te/extend should be adding everything to env.
    `(fn (: ,v ,type) ,(casto body (cons (cons v type) te))))

   ((,rator ,rand)
    (let* ((rator-cast (begin (display "casting rator") (newline)
			      (display (casto rator te)) (newline)
			      (casto rator te)))
	   (rator-type (type-of-cast rator-cast))
	   (rand-cast (casto rand te))
	   (rand-type (type-of-cast rand-cast)))
      (if (any-type? rator-type)
	  ;; function type is ?
	  ;; `(: ((: ,rator ,(make-arrow-type rand-type 'any)) ,rand-cast) any)
	  (if (pair? rator)
	      `(: (,rator-cast ,rand-cast) any)
	      `(: ((: ,rator ,(make-arrow-type rand-type 'any)) ,rand-cast) any))
	  (if (equal? rand-type (domain rator-type))
	      (if (pair? rator)
		  `(: (,rator-cast ,rand-cast) ,(co-domain rator-type))
		  `(: ((: ,rator ,rator-type) ,rand-cast) ,(co-domain rator-type)))
	      (if (~ (domain rator-type) rand-type)
		  (if (pair? rator)
		      `(: (,rator-cast (: ,rand ,(domain rator-type))) ,(co-domain rator-type))
		      `(: ((: ,rator ,rator-type) (: ,rand ,(domain rator-type))) ,(co-domain rator-type)))
		  (error "CastError -- " expr))))))
   
   (,e (guard (number? e)) `(: ,e number))
   (,e (guard (symbol? e)) `(: ,e symbol))
   (,e (guard (char? e)) `(: ,e char))
   (,e (guard (boolean? e)) `(: ,e boolean))
   (else `(: ,expr ,(car (dispatch-tag-contents (dispatch-tag expr)))))))


;; Idea here was to return a table of <symbol><type> pairs rather than
;; modify the expression to be casted
(define (cast-tbl expr te)
  (pmatch
   expr
   (,e (guard (symbol? e))
       (let ((val (te/lookup te expr)))
	 (if val
	     te
	     (te/extend te e 'any))))
   ((fn (: ,v ,type) ,body)
    (cast-tbl body (te/extend te v type)))
   ((,rator ,rand)
    (let* ((rator-type (te/lookup te rator))
   	   (rand-cast (cast-tbl rand te))
   	   (rand-type (te/lookup rand-cast rand)))
      (if (not rator-type)
   	  (te/extend rand-cast rator (make-arrow-type rand-type 'any))
   	  (begin (display rator) (display " in te") (newline)
   		 (if (equal? rand-type (domain rator-type))
   		     (te/extend rand (domain rantor-type))
   		     (if (~ (domain rator-type) rand-type)
   			 (te/extend rand rand-type)
   			 'cast-error))))))))

;; Second version of cast
(define (cast-expr expr te)
  (pmatch
   expr
   (,e (guard (symbol? e))
       (let ((typed? (te/lookup te e)))
	 (if typed?
	     (cast-symbol e typed?)
	     (cast-symbol e 'any))))
   ((lambda (,v) ,body)
    (let* ((v-cast (cast-expr v te))
	   (te2 (te/extend te v (type-of-cast v-cast))))
      `(fn ,v-cast ,(cast-expr body te2))))
   ((fn (: ,v ,type) ,body)
    `(fn (: ,v ,type) ,(cast-expr body (te/extend te v type))))

   ;; ((,func)
   ;;  (let ((func-type (caddr (cast-expr func te))))
   ;;    (cast-symbol func (co-domain func-type))))
   
   ((,rator ,rand)
    ;; rator type doesn't get cast properly
    (let* ((rator-type (te/lookup te rator))
	   (rand-cast (cast-expr rand te))
	   (rand-type (type-of-cast rand-cast)))
      (if (not rator-type)
	  `(: (,(cast-symbol rator (make-arrow-type rand-type 'any)) ,rand-cast) any)
	  (if (equal? rand-type (domain rator-type))
	      `(: (,(cast-symbol rator rator-type) ,rand-cast) ,(co-domain rator-type)) 
	      (if (~ (domain rator-type) rand-type)
		  `(: (,(cast-symbol rator rator-type)
		       (,(cast-symbol rand (domain rator-type)))) ,(co-domain rator-type))
		  'cast-error)))))
   
   ((,rator . ,rands) (guard (predefined? rator))
    (error "CAST -- PREDEFINED -- Not implemented yet"))

   ((,rator . ,rands)
    (error "CAST -- N-ARY APP -- Not implemented yet"))
   ;; See if you can avoid all these clauses

   (,e (guard (number? e)) (cast-symbol e 'number))

   (,e (guard (string? e)) (cast-symbol e 'string))

   (,e (guard (char? e)) (cast-symbol e 'char))

   (,e (guard (boolean? e) (cast-symbol e 'boolean)))

   (else (cast-symbol expr (car (dispatch-tag-contents (dispatch-tag expr)))))))


;; First version of cast and uncast
(define (cast expr te)
  (pmatch
   expr
   (,e (guard (predefined? e))
       `(: ,e ,(get-predefined-type e)))
   (,e (guard (symbol? e))
       (let ((type (te/lookup te e)))
	 (if type
	     `(: ,e ,type)
	     `(: ,e any))))
   ((lambda (,v) ,body)
    (let* ((v-cast (cast v te))
	   (te2 (te/extend te v (caddr v-cast))))
      ;; `(typed-lambda ,v-cast ,(map (lambda (x) (cast x te2)) body))
      `(typed-lambda ,v-cast ,(cast body te2))
      ))
   ((lambda ,vs ,body)
    (let* ((v-cast (map (lambda (x) (cast x te)) vs)))
      `(typed-lambda ,v-cast ,(cast body te))))
   ((,rator . ,rands)
    (let* ((r-cast (cast rator te))
	   (te2 (te/extend te rator (caddr r-cast))))
      `(,r-cast ,@(map (lambda (x) (cast x te2)) rands))))
   (else `(: ,expr ,(car (dispatch-tag-contents (dispatch-tag expr)))))))


(define (uncast expr)
  (pmatch
   expr
   ((: ,e ,type) e)
   ((typed-lambda (: ,v ,type) ,body)
    `(lambda (,v) ,(uncast body)))
   ((,e . ,es)
    (cons (uncast e) (uncast es)))
   (else expr)))










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
	 (else (error "Not implemented yet"))))))(define (tco expr te)
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
