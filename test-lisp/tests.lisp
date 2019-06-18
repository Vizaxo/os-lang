(load-file "examples/stdlib.lisp")


((lambda (x) 'passed) 'failed-lambda-const)

((lambda (x) x) 'passed)

((lambda xs (car xs)) 'passed 'failed-lambda-varargs)

(apply (lambda xs (car xs)) '(passed failed-apply-lambda-varargs))

(eval ''passed)

(defun last (xs)
  (if (cdr xs)
      (last (cdr xs))
    (car xs)))

(last '(passed))
(last '(first second third passed))

(last (list 'first 'second 'passed))

(match-list '()
            'passed
            'cons-case)

(match-list '(a b c)
            'nil-case
            (lambda (hd tl) (if (eq? hd 'a)
                           (if (eq? tl '(b c))
                               'passed
                             'tail-not-b-c)
                         'head-not-a)))

(if (eq? (map (lambda (x) (list x 'xyz)) '(a b c)) '((a xyz) (b xyz) (c xyz)))
    'passed
  'failed-map-xyz)

(cadr '(failde-cadr-first passed failed-cadr-third))

(car (cddr '(failed-cddr-first failde-cddr-second passed failed-cddr-fourth)))

(cond ('() 'failed-cond-a)
      ('() 'failed-cond-b)
      ('t 'passed)
      ('() 'failed-cond-c))
