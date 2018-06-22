(load "pmatch.scm")
(load "types.scm")

(type-alias 'Envr '(list (pair any any)))
(type-alias 'Expr '(list any))

;; typed selectors
(defn (: value (-> Expr any)) (exp)
   (car (cdr exp)))

(defn (: type (-> Expr any)) (exp)
   (car exp))

;; untyped selectors
(define operator cadr)
(define operand caddr)
(define param cadr)
(define body caddr)

(defn (: eval (-> (* Expr Envr) any)) (exp env)
  (if (eq? (type exp) 'var)
      (cdr (assoc (value exp) env))
      (if (eq? (type exp) 'int)
          (value exp)
          (if (eq? (type exp) 'app)
              (apply
                (eval (operator exp) env)
                (eval (operand exp) env))
              (if (eq? (type exp) 'lam)
                  (listof (: any)
                    (param exp) (body exp) env)
                  (listof (: any)))))))

;; untyped apply
(define (apply f arg)
  (pmatch f
    ((,x ,body ,env)
     (eval body (cons (cons x arg) env)))
    (else (error "attempting to apply non-function"))))


