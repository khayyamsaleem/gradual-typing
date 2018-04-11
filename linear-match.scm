;;;; Pattern Matcher
;;; http://okmij.org/ftp/Scheme/macros.html

(define-syntax match
  (syntax-rules ()
    ((_ exp clause ...)
     (let ((val-to-match exp))
       (match-case-simple* val-to-match clause ...)))))

(define (match-failure val)
  (error "failed match" val))

(define-syntax match-case-simple*
  (syntax-rules (else)
    ((_ val (else exp ...))
     (let () exp ...))
    ((_ val)
     (match-failure val))
    ((_ val (pattern () exp ...) . clauses)
     (let ((fail (lambda () (match-case-simple* val . clauses))))
       ;; note that match-pattern may do binding. Here,
       ;; other clauses are outside of these binding.
       (match-pattern val pattern (let () exp ...) (fail))))
    ((_ val (pattern guard exp ...) . clauses)
     (let ((fail (lambda () (match-case-simple* val . clauses))))
       (match-pattern val pattern
		      (if guard (let () exp ...) (fail))
		      (fail))))))

(define-syntax match-pattern
  (syntax-rules (__ quote unquote)
    ((_ val __ kt kf) kt)
    ((_ val () kt kf)
     (if (null? val) kt kf))
    ((_ val (quote lit) kt kf)
     (if (equal? val (quote lit)) kt kf))
    ((_ val (unquote var) kt kf)
     (let ((var val)) kt))
    ((_ val (x . y) kt kf)
     (if (pair? val)
	 (let ((valx (car val))
	       (valy (cdr val)))
	   (match-pattern valx x
			  (match-pattern valy y kt kf)
			  kf))
	 kf))
    ((_ val lit kt kf)
     (if (equal? val (quote lit)) kt kf))))
