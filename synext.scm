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
    ((_ (: type) . items)
     (quote items))))

(define-syntax defvar
  (syntax-rules (:)
    ((_ (: name type) val)
     (environment-define (the-environment) 'name val))))

(define-syntax defn
  (syntax-rules (:)
    (error "Not implemented yet")))

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



