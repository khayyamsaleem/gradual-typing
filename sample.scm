(fn (: x number) x)

(fn (: x number)
    (if (> x 0)
	#t
	#f))

(fn (: x number)
    (listof (: number) x x (* x x)))

(if (> x 0)				; shouldn't check this
    #t
    "false")

(fn (: x number)
    (if (> x 0)
	#t
	#f))

(defvar (: succ (-> number number)) (fn (: x number) (+ x 1)))

((fn (: x number) (succ x)) 3)

(succ 3)

(define (square x)
  (* x x))

(defvar (: t number) 10)

(square t)

(listof (: number) 1 2 3 t)

(defn (: succ (-> number number)) (x)
  (+ x 1))
