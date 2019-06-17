(def list (lambda args args))

(def defmacro
     (mac (name args body)
          (list 'def name (list 'mac args body))))

(def Z (lambda (f)
         ((lambda (g)
            (f (g g)))
          (lambda (g)
            (f (lambda x (apply (g g) x)))))))

(def defun
     (mac (name args body)
          (list 'def name
                (list 'Z (list 'lambda (list name)
                               (list 'lambda args body))))))

(defun match-list (xs nilcase conscase)
  (if xs
      (conscase (car xs) (cdr xs))
    nilcase))

(defun map (f xs)
  (match-list xs '() (lambda (hd tl) (cons (f hd) (map f tl)))))

