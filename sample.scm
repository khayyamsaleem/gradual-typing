(fn (: x number) x)

(fn (: x number) (listof (: number) x x (* x x)))

(defvar (: succ (-> number number)) (fn (: x number) (+ x 1)))

((fn (: x number) (succ x)) 3)

((fn (: x any) (succ x)) #t)

(succ 3)

(define (square x) (* x x))

(defvar (: t number) 10)

(square t)

(listof (: number) 1 2 3 t)

(defn (: succ (-> number number)) (x)
  (+ x 1))

