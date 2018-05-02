(load "pmatch.scm")

#|
	Types
|#

(define (any-type? type)
  (eq? type 'any))

(define (unit-type? type)
  (eq? type 'unit))

(define (make-arrow-type s t)
  `(-> ,s ,t))

(define (make-arrow-type-arity s t arity)
  (if (and (number? arity) (<= arity 1))
      (make-arrow-type s t)
      `(,(symbol-append '-> arity) ,s ,t)))

(define (arrow-type? type)
  (and (pair? type) (symbol? (car type)) (substring? "->" (symbol->string (car type)))))

;; (define (domain arrow-type)
;;   (if (any-type? arrow-type)
;;       'any
;;       (cadr arrow-type)))

(define (domain arrow-type)
  (cond ((any-type? arrow-type) 'any)
	((eq? (arity arrow-type) 'n) (cadr arrow-type))
	((= (arity arrow-type) 1) (cadr arrow-type))
	(else (cadr (nary->pair arrow-type)))))

(define (co-domain arrow-type)
  (if (any-type? arrow-type)
      'any
      (caddr arrow-type)))

(define (arity arrow-type)
  (if (arrow-type? arrow-type)
      (if (> (string-length (symbol->string (car arrow-type))) 2)
	  (let ((ari (string-ref (symbol->string (car arrow-type)) 2)))
	    (if (eq? ari #\n)
		'n
		(char->digit ari)))
	  1)
      (error "ARITY -- input is not a function type, " arrow-type)))

(define (pair-type? type)
  (and (pair? type) (eq? (car type) '*)))

(define (pair-arity type)
  (and (pair-type? type) (length (cdr type))))

(define (make-pair-type p1 p2 . rest)
  `(* ,p1 ,p2 ,@rest))

(define (list->pair-type ls)
  `(* ,@ls))

(define (nary->pair arrow-type)
  (define (repeat symbol times)
    (if (zero? times) '() (cons symbol (repeat symbol (- times 1)))))
  (if (arrow-type? arrow-type)
      (let ((ar (arity arrow-type)))
	(if (or (eq? ar 'n) (= 1 ar))
	    arrow-type
	    (make-arrow-type
	     `(* ,@(repeat (cadr arrow-type) ar))
	     (co-domain arrow-type))))
      arrow-type))

(define (pair->nary type)
  (define (all-equal? ls)
    (or (or (null? ls) (null? (cdr ls)))
	(and (equal? (car ls) (cadr ls))
	     (all-equal? (cdr ls)))))
  (if (arrow-type? type)
      (let ((d (domain type)))
	(if (and (pair-type? d) (all-equal? (cdr d)))
	    (make-arrow-type-arity (cadr d) (co-domain type) (pair-arity d))
	    type))
      type))

(define (make-list-type base-type)
  `(list ,base-type))

(define (list-type? type)
  (and (pair? type) (eq? 'list (car type))))

(define (base-type? val)
  (or (number? val)
      (string? val)
      (char? val)
      (boolean? val)))

(define predefined-types
  '((+ . (->n number number))
    (- . (->n number number))
    (* . (->n number number))
    (/ . (->n number number))
    (< . (->n number boolean))
    (> . (->n number boolean))
    (= . (->n number boolean))
    (>= . (->n number boolean))
    (<= . (->n number boolean))
    (eq? . (->2 any boolean))
    (eqv? . (->2 any boolean))
    (equal? . (->2 any boolean))
    (null? . (-> (list any) boolean))
    (cons . (-> (* any (list any)) (list any)))
    (car . (-> (list any) any))
    (cdr . (-> (list any) (list any)))
    (map . (-> (* (-> any any) (list any)) (list any)))))

(define (predefined? type)
  (assoc type predefined-types))

(define (get-predefined-type symbol)
  (let ((type (assoc symbol predefined-types)))
    (if type
	(cdr type)
	'any)))

;; (define (~ s t)
;;   (cond ((or (any-type? s) (any-type? t)))
;; 	((equal? s t))
;; 	((and (pair? s) (pair? t))
;; 	 (and (~ (car s) (car t))
;; 	      (~ (cdr s) (cdr t))))
;; 	(else #f)))

;; ~ : any type and pair type are not consistent. This is done
;;     to deal with functions of multiple arity
(define (~ s t)
  (cond ((any-type? s) (not (pair-type? t)))
	((any-type? t) (not (pair-type? s)))
	((equal? s t))
	((and (arrow-type? s) (arrow-type? t))
	 (and (~ (car (nary->pair s)) (car (nary->pair t)))
	      (~ (cdr (nary->pair s)) (cdr (nary->pair t)))))
	((and (pair? s) (pair? t))
	 (and (~ (car s) (car t))
	      (~ (cdr s) (cdr t))))
	(else #f)))

;; (define (te/extend te var type)
;;   (if (not (te/lookup te var))
;;       (cons (cons var type) te)
;;       te))

;; (define (te/extend te var type)
;;   (cons (cons var type) te))

;; (define (te/lookup te var)
;;   (let ((type (assoc var te)))
;;     (if type (cdr type) #f)))
