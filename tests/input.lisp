(define fib
  (lambda (n)
    (cond ((<= n 2) 1)
          ('t (+ (fib (- n 1)) (fib (- n 2)))))))
(fib 20)
