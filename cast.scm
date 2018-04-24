#|
	Intermediate Typed Language
|#

(load "pmatch.scm")
(load "types.scm")

(define-syntax fn
  ;;  curried
  (syntax-rules (:)
    ((_ (: v type) body ...)
     (lambda (v) body ...))
    ((_ ((: v type) v2 ...) body ...)
     (lambda (v) (fn (v2 ...) body ...)))
    ((_ () body ...)
     (lambda () body ...))))

(define (cast-symbol e type)
  `(: ,e ,type))

(define (cast? expr)
  (and (pair? expr) (eq? (car expr) :)))

(define (type-of-cast expr)
  (caddr expr))

(define (expr-of-cast expr)
  (cadr expr))

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

;; Scrap
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

(define call/cc call-with-current-continuation)

(define (castu expr te)
  (define (make-cast expr type)
    `(: ,expr ,type))
  (define (un-cast cast)
    (and (eq? (car cast) ':) (cadr cast)))
  (pmatch
   expr
   (,e (guard (symbol? e))
       (let ((typed? (te/lookup te e)))
	 (if typed? (make-cast e typed?) (make-cast e 'any))))
   
   ((fn (: ,v ,type) ,body)
    (let* ((body-cast (castu body (cons (cons v type) te)))
	   (co-domain-type (type-of-cast body-cast)))
      (make-cast `(fn (: ,v ,type) ,body-cast) (make-arrow-type type co-domain-type))))
   
   ((fn (: ,v ,type) (: ,ret-type) ,body)
    (let* ((body-cast (castu body (cons (cons v type) te)))
	   (co-domain-type (type-of-cast body-cast)))
      (if (~ co-domain-type ret-type)
	  (make-cast `(fn (: ,v ,type) ,body-cast) (make-arrow-type type ret-type))
	  (error "CastError --" expr))))
   
   ((,rator ,rand)
    (let* ((rator-cast (castu rator te))
	   (rator-type (type-of-cast rator-cast))
	   (rand-cast (castu rand te))
	   (rand-type (type-of-cast rand-cast)))
      (if (any-type? rator-type)
	  (make-cast (list (make-cast (un-cast rator-cast) (make-arrow-type rand-type 'any))
			   rand-cast) 'any)
	  (if (equal? (domain rator-type) rand-type)
	      (make-cast (list rator-cast rand-cast) (co-domain rator-type))
	      (if (~ (domain rator-type) rand-type)
		  (make-cast (list rator-cast (make-cast (un-cast rand-cast) (domain rator-type)))
			     (co-domain rator-type))
		  (error "CastError --" expr))))))
   
   (,e (guard (number? e)) (make-cast e 'number))
   (,e (guard (string? e)) (make-cast e 'string))
   (,e (guard (boolean? e)) (make-cast e 'boolean))
   (,e (make-cast e (car (dispatch-tag-contents (dispatch-tag e)))))
   (else (error "Not implemented"))))



