(def let1
     (mac (name val body)
          (cons (cons 'lambda (cons (cons name '()) (cons body '()))) (cons val '()))))

(def Z (lambda (f)
         ((lambda (g)
            (f (g g)))
          (lambda (g)
            (f (lambda (x) ((g g) x)))))))

((Z (lambda (last)
      (lambda (x)
        (if (cdr x)
            (last (cdr x))
          x))))
 '(fi fi fo fum))

(def varadic (lambda args (car (cdr args))))

(varadic 'a 'b 'c 'd)

(def varmacro
     (mac args
          (cons 'quote (cons (car (cdr args)) '()))))

(varmacro 'a b 'c 'd)
