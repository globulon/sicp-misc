#lang scheme

(define eval-table (make))

(define (get from-exp) (hash-ref eval-table from-exp #f))

(define (set! exp f) (hash-set! eval-table exp f))

(define (evaluator-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp)) 
         ((get (car exp)) exp env))
        ((application? exp)
         (evaluator-apply (eval (operator exp) env)
                          (list-of-values (operands exp) env)))
        (else 
         (error "Unknown expression type -- EVAL" exp))))

        ((definition? exp) (eval-definition exp env))
        
(set! 'define 
      (lambda (exp env)
        (eval-definition exp env)))        

(set! 'quote 
      (lambda (exp env)
        (text-of-quotation exp)))

(set! 'set!
      (lambda (exp env)
        (eval-assignement exp env)))

(set! 'begin
      (lambda (exp env)
        (eval-sequence (begin-actions exp)env)))

(set! 'if 
      (lambda (exp env)
        (eval-if exp env)))

(set! 'cond
      (lambda (exp env)
        (eval (cond->if exp) env)))

(set! 'lambda
      (lambda (exp env)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                         env)))

              
