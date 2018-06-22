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

We have a type consistency operator <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/39336a2ffd276833bc2af414ed460bfa.svg?d40937ed9a&invert_in_darkmode" align=middle width=12.7854342pt height=14.1552444pt/>. <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/39336a2ffd276833bc2af414ed460bfa.svg?24d7c770eb&invert_in_darkmode" align=middle width=12.7854342pt height=14.1552444pt/> is symmetric and reflexive, but not transitive.
<img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/cf37b5c0b85c89a22141a8acb12544cb.svg?2802e78ac0&invert_in_darkmode" align=middle width=7.76259pt height=22.8310566pt/> is <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/39336a2ffd276833bc2af414ed460bfa.svg?9d9723a491&invert_in_darkmode" align=middle width=12.7854342pt height=14.1552444pt/> with every other type.

The typing rules we implement are essentially,

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/2efc983447c66fb611c01b1c1e0a909e.svg?767456e5ee&invert_in_darkmode" align=middle width=65.59352085pt height=34.7253258pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/c6cd8096a960aed249b0b52b2a552bd1.svg?77e08b7265&invert_in_darkmode" align=middle width=59.310966pt height=33.62942055pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/7cb6f329a9f70816714440b00b248293.svg?28f2245496&invert_in_darkmode" align=middle width=141.72313485pt height=34.7253258pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/071219fcffda373da462a0d031274568.svg?c39dac95bc&invert_in_darkmode" align=middle width=153.29281275pt height=36.2778141pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/802367a8916bb01390be22fa3824a3d0.svg?1e8939f7df&invert_in_darkmode" align=middle width=376.431627pt height=36.2778141pt/></p>

The condition <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/429e4ec21ca5eb5ac67224ede0d5d1b6.svg?6e015da769&invert_in_darkmode" align=middle width=100.7227089pt height=22.8310566pt/> ensures that we cannot apply a function whose
domain is not <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/cf37b5c0b85c89a22141a8acb12544cb.svg?7ad217684e&invert_in_darkmode" align=middle width=7.76259pt height=22.8310566pt/> to a value of type <img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/cf37b5c0b85c89a22141a8acb12544cb.svg?88cb2589aa&invert_in_darkmode" align=middle width=7.76259pt height=22.8310566pt/>. This is inspired by Typed Racket:

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

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/43747241ae48c9922ae99f4b43f47b2f.svg?ec5f28f9f&invert_in_darkmode" align=middle width=275.8424658pt height=17.0840637pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/e4d527cb838383fdcdf2a70d22087517.svg?5e9b6b0817&invert_in_darkmode" align=middle width=344.8829538pt height=17.0840637pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/75edb5dc7bf77df80db970367cdfa039.svg?4b4b0e51cd&invert_in_darkmode" align=middle width=293.10257625pt height=17.0840637pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/00d34c8d8d041e651d3f1dbfce32f6ed.svg?8c11ed1c3b&invert_in_darkmode" align=middle width=198.17188545pt height=19.3672215pt/></p>

<p align="center"><img src="https://rawgit.com/khayyamsaleem/cs-810-gradual-typing/None/svgs/f799db6ab73c41d9f36eb2f2ac8245bf.svg?93c4e3333e&invert_in_darkmode" align=middle width=465.70385565pt height=19.3672215pt/></p>

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
