#lang racket
((lambda (n)
  ((lambda (fact)
     (fact fact n))
   (lambda (fact k)
     (if (= 1 k)
         k
         (* k (fact fact (- k 1)))))))
  10)

((lambda (n)
   ((lambda (fibo)
     (fibo fibo n '(1 0)))
    (lambda (fibo remaining acc)
      (if (zero? (- remaining 2))
          acc
          (fibo fibo 
                (- remaining 1) 
                (cons (+ (car acc) (cadr acc))
                      acc))))))
 20)

((lambda (n)
   ((lambda (even? odd?)
      (even? even? odd? n))
    (lambda (even? odd? k)
      (if (zero? k)
          true
          (odd? odd? even? (- k 1))))
    (lambda (odd? even? k)
      (if (zero? k)
          false
          (even? even? odd? (- k 1))))))    
 4)
