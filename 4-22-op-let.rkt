#lang racket
;(define (eval exp env)
;  ((analyze exp) env))

(define (analyze exp) '())

;(define (analyze exp)
;  ((let? exp)
;    (analyze-let exp)))
;;easy version 
;;we expand the expression as
;;lambda application and ask for analyze

(define (analyze-let exp)
  (analyze let->combination))

(define (let->combination exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (let ((params (map car bindings))
          (exps (map cadr bindings)))
      (cons
       (make-lambda params body)
       exps))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
