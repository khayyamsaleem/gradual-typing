# gradual-typing

A small gradual type system for Scheme.

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/cleanup/svgs/32737e0a8d5a4cf32ba3ab1b74902ab7.svg?invert_in_darkmode" align=middle width=127.9847844pt height=39.452455349999994pt/></p>

## about

TODO

## typing rules

TODO

## examples

### type checking

The function `t` in `caster.scm` takes a quoted expression and a typing context
and type checks the expression. The typing context is represented using a hash
table, but the `t` function accepts an association list for convenience.

#### `fn` - typed `lambda`

```scheme
(t '(fn (: x number) x) '())
;; Γ ▷ (fn (: x number) x) : (-> number number)

(t '(fn (: x number) (string? x))
   '(string? . (-> string boolean)))
;; TypeError : expected string, got number for x

(t '(fn (: x number) (: string) y) '())
;; Γ {(y . any)} 
;;   ▷ (fn (: x number) (: string) y) : (-> number string)

(t '(fn (: x number) (: string) x) '())
;; TypeError : expected string, got number for x
```

#### `listof` - homogeneous lists

```scheme
(t '(listof (: number) x y z) '())
;; Γ {((z . any) (x . any) (y . any))} 
;;   ▷ (listof (: number) x y z) : (list number)

(t '(listof (: number) x y z) '((z . string)))
;; TypeError : expected string, got number for z
```

#### function application

```scheme
(t '(f x) '())
;; Γ {((x . any) (f . (-> any any)))} 
;;   ▷ (f x) : any

(t '(f x) '((x . number)))
;; Γ {((x . number) (f . (-> number any)))}
;;   ▷ (f x) : any

(t '(f x) '((x . number) (f . (-> string number))))
;; TypeError : expected string, got number for x

(t '(f x) '((f . (-> (* number string) boolean))))
;; TypeError : expected (* number string), got any for x
```

Recall the typing rule for function application that doesn't allow
the argument to be of type `any` if the domain of the function
is not `any`.

#### general examples

A few situations this type system helps avoid.

```scheme
(t '(fn (: x number) (f x)) '((f . (-> string any))))
;; TypeError : expected string, got number for x

(t '(fn (: x any) (succ x)) '((succ . (-> number number))))
;; TypeError : expected number, got any for x

(t '(fn (: x boolean) (: string) (f x))
   '((f . (-> boolean number))))
;; TypeError : expected string, got number for (f x)

(t '(f (f x)) '((f . (-> number string)) (x . number)))
;; TypeError : expected number, got string for (f x)
```

### λ

A gradually typed interpreter for the λ-calculus

#### code

```scheme
;; file: interp.scm
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

;; type checked eval. We aren't type checking `cond' yet.
(defn (: teval (-> (* Expr Envr) any)) (exp env)
  (if (eq? (type exp) 'var)
      (cdr (assoc (value exp) env))
      (if (eq? (type exp) 'int)
          (value exp)
          (if (eq? (type exp) 'app)
              (tapply
                (teval (operator exp) env)
                (teval (operand exp) env))
              (if (eq? (type exp) 'lam)
                  (listof (: any)
                    (param exp) (body exp) env)
                  (listof (: any)))))))

;; untyped apply
(define (tapply f arg)
  (pmatch f
    ((,x ,body ,env)
     (teval body (cons (cons x arg) env)))
    (else (error "attempting to apply non-function"))))
```

#### usage

The `type-check` function takes in a filename and runs the type
checker on annotated S-expressions in that file. The `predefined-types`
typing context found in `types.scm` is used.

Note: The `type-alias` system isn't entirely usable. These additional lines
have to be entered into the REPL after loading `input.scm`

```scheme
(load "input.scm")
(type-alias 'Envr '(list (pair any any)))
(type-alias 'Expr '(list any))
```

```scheme
(type-check "interp.scm")

;; when successful

(load "interp.scm")

(teval '(int 3) '())
;; => 3

(teval '(app (lam x (var x)) (int 10)) '())
;; => 10

(teval '(app (lam x (lam y (var x)))
             (lam z (var z)))
       '())
;; => (y (var x) ((x z (var z) ())))
```
