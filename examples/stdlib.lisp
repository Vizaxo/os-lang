(def defmacro
     (mac (name args body)
          `(def ,name (mac ,args ,body))))

(def Z (lambda (f)
         ((lambda (g)
            (f (g g)))
          (lambda (g)
            (f (lambda x (apply (g g) x)))))))

(def defun
     (mac (name args body)
          `(def ,name
                (Z (lambda (name)
                     (lambda ,args ,body))))))

(defun list args
  args)

(defun match-list (xs nilcase conscase)
  (if xs
      (conscase (car xs) (cdr xs))
    nilcase))

(defun map (f xs)
  (match-list xs '() (lambda (hd tl) (cons (f hd) (map f tl)))))

(defun cadr (xs)
  (car (cdr xs)))

(defun cddr (xs)
  (cdr (cdr xs)))
