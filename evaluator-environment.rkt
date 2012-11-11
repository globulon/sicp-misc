#lang planet neil/sicp

(define (empty? exp) (null? exp))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars args env)
  (cond ((= (length vars) (length args))
         (cons (make-frame vars args) env))
        ((< (length vars) (length args))
         (error "missing variable names -- XTEND-ENV"))
        (else 
         (error "missing arguments -- XTEND-ENV"))))

(define (env-loop callback action var e)
  (define (scan vars vals)
    (cond ((empty? vars) 
           (callback e))
          ((eq? (car vars) var) 
           (action vals))
          (else 
           (scan (cdr vars) (cdr vals)))))
  (if (eq? the-empty-environment e)
      (error "variable not bound --" var)
      (let ((frame (first-frame e)))
        (let ((vars (frame-variables frame))
              (vals (frame-values frame)))
          (scan vars vals)))))
  
(define (lookup-variable-value var env)
  (define (callback e)
    (env-loop callback car var (enclosing-environment e)))
  (env-loop callback car var env))

(define (set-variable-value! var val env)
  (define (callback e)
    (env-loop callback car var (enclosing-environment e)))
  (define (update vals)
    (set-car! vals val))  
  (env-loop callback update var env))

(define (set-definition-value! var val env)
  (define (callback e)
    (add-binding-to-frame var val (first-frame e)))
  (define (update vals)
    (set-car! vals val))
  (env-loop callback update var env))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (set-up-env)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (set-definition-value! 'true  true initial-env)
    (set-definition-value! 'false false initial-env)
    initial-env))

(define global-env (set-up-env))
