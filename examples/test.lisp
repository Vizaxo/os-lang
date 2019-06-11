(def let1
     (mac (name val body)
          (cons (cons 'lambda (cons (cons name '()) (cons body '()))) (cons val '()))))

(def Z (lambda (f)
         ((lambda (g)
            (f (g g)))
          (lambda (g)
            (f (lambda (x) ((g g) x)))))))

(def list (lambda args args))

(def defmacro
     (mac (name args body)
          (list 'def name (list 'mac args body))))

(def defun
     (mac (name args body)
          (list 'def name (list 'lambda args body))))

