(load "pmatch.scm")
(load "types.scm")
(load "synext.scm")

(define (square x)
  (* x x))

(defvar (: t1 (list number))
  (listof (: number) 1 2 3 4 5))

(defn (: range (-> (* number number) (list number))) (x y)
  (if (> x y)
      (listof (: number))
      (cons x (range (+ x 1) y))))

(defvar (: s (list number)) (range 1 10))

(defvar (: s1 (list string)) (map (fn (: x number) (square x)) s))

(defn (: fact (-> number number)) (x)
  (if (< x 1)
      1
      (* x (fact (- x 1)))))

(defvar (: t1 number) (fact 5))

(defn (: tappend (-> (* (list any) (list any)) (list any))) (x y)
  (if (null? x)
      y
      (cons (car x) (tappend (cdr x) y))))

(define s (tappend '(1 2 3 4) '(4 5 6 6)))
