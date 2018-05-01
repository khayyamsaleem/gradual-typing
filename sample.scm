(fn (: x number) x)

(fn (: x number)
    (if (> x 0)
	#t
	#f))

(fn (: x number)
    (listof (: number) x x (* x x)))

(if (> x 0)
    #t
    "false")

(fn (: x number)
    (if (> x 0)
	#t
	#f))

(lambda (x) x)

(let ((x 3))
  ((lambda (x) (+ x 3)) x))
