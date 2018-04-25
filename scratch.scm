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
