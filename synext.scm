#|
	Syntactic Extensions
|#

(define-syntax fn-erase
  (syntax-rules (:)
    ((_ (: v type))
     '(v))
    ((_ ((: v type) v2 ...))
     `(v ,@(fn-erase v2 ...)))))

(define-syntax fn
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

(define-syntax listof
  (syntax-rules (:)
    ((_ (: type) e1 ...)
     (list e1 ...))))

(define-syntax pair
  (syntax-rules ()
    ((_ e1 e2)
     (cons e1 e2))))

(define-syntax defvar
  (syntax-rules (:)
    ((_ (: name type) val)
     (define name val))))

(define-syntax defn
  (syntax-rules (:)
    ((_ (: name type) (arg1 . args) body ...)
     (define (name arg1 . args) body ...))))

#|
	REPL specific
|#

(define-syntax >>
  ;; Need to fix
  (syntax-rules ()
    ((_ fn . args)
     (begin
       (display '(fn . args)) (newline)
       (apply fn (quote args))))))
