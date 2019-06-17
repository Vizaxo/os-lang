(def let1
     (mac (name val body)
          (cons (cons 'lambda (cons (cons name '()) (cons body '()))) (cons val '()))))

(def list (lambda args args))

(def defmacro
     (mac (name args body)
          (list 'def name (list 'mac args body))))

(def defun
     (mac (name args body)
          (list 'def name (list 'lambda args body))))

(def Z (lambda (f)
         ((lambda (g)
            (f (g g)))
          (lambda (g)
            (f (lambda x (apply (g g) x)))))))

(defun swap (x y)
  (list y x))

(defun uncons (xs nilcase conscase)
  (if xs
      (conscase (car xs) (cdr xs))
    nilcase))

(def map
     (Z (lambda (map)
          (lambda (f xs)
            (uncons xs '() (lambda (hd tl) (cons (f hd) (map f tl))))))))

(map (lambda (x) (list x 'hi)) '(a b c))

