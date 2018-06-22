# gradual-typing

A small gradual type system for Scheme.

## about

## typing rules

## usage

## example

A gradually typed interpreter for the lamda calculus

```scheme
(type-alias 'Envr '(list (pair any any))) ;; an environment
(type-alias 'Expr '(list any))            ;; an expression

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

;; type checked eval
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


```
