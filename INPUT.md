# gradual-typing

## about

Gradual Typing allows some parts of a program to be dynamically typed and other parts to
be statically typed. Type annotations provided by the programmer determine which portions
of the program are type checked before execution.

This is an implementation of a subset of the system described in the paper [*Gradual Typing for Functional
Languages* by Siek and Taha (2006)](https://www.cs.indiana.edu/~lkuper/talks/gradual/gradual.pdf).

## gradual typing - rules

We first cast Scheme terms into an intermediate language in which all terms are fully annotated. Type checking
is then performed on this intermediate language. Consult the paper referenced and the documents in `./report`
for more information. 

We have a type consistency operator $\sim$. $\sim$ is symmetric and reflexive, but not transitive.
$?$ is $\sim$ with every other type.

The typing rules we implement are essentially,

$$
\frac{\Gamma x = \lfloor x \rfloor}{\Gamma \vdash x : \tau}
$$

$$
\frac{\Delta c = \tau}{\Gamma \vdash c : \tau}
$$

$$
\frac{\Gamma (x \mapsto \sigma) \vdash e : \tau}{\Gamma \vdash \lambda x : \sigma . e : \sigma \rightarrow \tau}
$$

$$
\frac{\Gamma \vdash e_{1} : \, ? \quad \Gamma \vdash e_{2} : \tau_{2}}{\Gamma \vdash e_{1} e_{2} : \, ?}
$$

$$
\frac{\Gamma \vdash e_{1} : \sigma \rightarrow \tau \quad \Gamma \vdash e_{2} : \sigma_{2} \quad \sigma \sim \sigma_{2} \quad \sigma \neq \, ? \wedge \sigma_{2} \neq \, ?}{\Gamma \vdash e_{1} e_{2} : \tau}
$$

The condition $\sigma \neq \, ? \wedge \sigma_{2} \neq \, ?$ ensures that we cannot apply a function whose
domain is not $?$ to a value of type $?$. This is inspired by Typed Racket:

```racket
((lambda ([x : Any]) (+ x 1)) #t)
;; Type Checker: type mismatch
;;   expected: Number
;;   given: Any
;;   in: x
```
In the example above, the function `+`'s domain is `Number` (presumably) but the value `x` has type `Any`.

## macros

We extend Scheme with new terms - `fn`, `listof`, `defvar`, `pair`, and `defn`.

```scheme
;; fn is typed lambda -- annotate parameter type and optionally, the return type
(fn (: <param> <type>) <body>)
(fn (: <param> <type>) (: <return-type>) <body>)

;; defvar is used to define new variables with types
(defvar (: <var> <type>) <value>)

;; listof is used to create typed lists
(listof (: <type>) <value>*)

;; pair is a macro used internally to deal with pair types. Might be removed later
(pair <fst> <snd>)

;; defn is used to define typed functions
(defn (: <function-name> <function-type>) (<params>) <body>)
```

### semantics

Once the type checking is done, we can get rid of the type annotations.

$$\frac{}{\texttt{(fn (: x s) M)} \rightarrow \texttt{(lambda (x) M)}}$$

$$\frac{}{\texttt{(listof (: s) m n ...)} \rightarrow \texttt{(list m n ...)}}$$

$$\frac{}{\texttt{(defvar (: x s) M)} \rightarrow \texttt{(define x M)}}$$

$$\frac{}{\texttt{(pair x y)} \rightarrow \texttt{(cons x y)}}$$

$$\frac{}{\texttt{(defn (: f s) (x y ...) M)} \rightarrow \texttt{(define (f x y ...) M)}}$$

## types

Types are represented using symbols. Here are the types we can work with in this system. The macros quote
the type parameter - we don't have to quote the types in the source.

```scheme
;;; dynamic types
any

;;; ground types
number
boolean
string
character

;;; pair types
(* <type> <type> ...)
;; e.g.
(* number string)
(* (-> boolean any) any (-> (-> string number) string))

;;; list types
(list <type>)
;; e.g.
(list number)
(list (-> number any))

;;; function types
(-> <domain> <co-domain>)
;; e.g.
(-> number number)
(-> number (-> string number))

;; multiple arity functions
(->k <dom> <cod>)
;; expands to (-> (* <dom> <dom> ... <dom>) <cod>)
;;                   ^-----  k times -----^
;; e.g.
(->2 number boolean)            ;; (-> (* number number) boolean)
(->3 string (-> string number)) ;; (-> (* string string string)
                                ;;     (-> string number))
;; special case
(->n <dom> <cod>) ;; can accept any number of <dom> types
                  ;; used to deal with scheme functions +, -, * and so on


;; types given to Scheme functions
(+ (->n number number))
(eq? (->2 any boolean))
(map (-> (* (-> any any) (list any)) (list any))) ;; more like map2
(assoc (-> (* any (list (pair any any))) (list (pair any any))))
```

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
