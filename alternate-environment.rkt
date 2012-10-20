#lang planet neil/sicp
;;alternate versions of 
;;environment for ex 4-11
(define (empty? exp) (null? exp))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (define (bind vars vals acc)
    (if (null? vars)
        acc
        (bind (cdr vars)
              (cdr vals)
              (cons (cons (car vars) (car vals))
                    acc))))
  (bind variables values '()))

;;could be a map hof
(define (collect select frame)
  (define (iter acc remaining)
    (if (null? remaining) 
        acc
        (iter (cons (select (car remaining)) acc)
              (cdr remaining))))
  (iter '() frame))

(define (frame-variables frame) 
  (collect car frame))

(define (frame-values frame) 
  (collect cdr frame))

(define (add-binding-to-frame var val frame)
  (cons (cons var val) frame))

(define (extend-environment vars args env)
  (cond ((= (length vars) (length args))
         (cons (make-frame vars args) env))
        ((< (length vars) (length args))
         (error "missing variable names -- XTEND-ENV"))
        (else 
         (error "missing arguments -- XTEND-ENV"))))

(define (lookup-variable-value var env)
  (define (scan pairs)
    (cond ((empty? pairs) 
           false)
          ((eq? (caar pairs) var) 
           (cdar pairs))
          (else 
           (scan (cdr pairs)))))
  (define (env-loop e)
    (if (eq? the-empty-environment e)
        (error "variable not bound -- LOOKUP VARIABLE VALUE" var))
          (let ((frame (first-frame e)))
            (let ((result (scan frame)))
              (if result
                  result
                  (env-loop (enclosing-environment e))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (update pairs)
    (cond ((empty? pairs) #f)
          ((eq? (caar pairs) var)
           (begin
             (set-cdr! (car pairs) val)
             #t))
          (else
           (update (cdr pairs)))))
  (define (env-loop e)
    (if (eq? the-empty-environment e)
        (error "variable not bound -- SET VARIABLE VALUE" var)
        (let ((frame (first-frame e)))
          (if (update frame)
              'done
              (env-loop (enclosing-environment env))))))
(env-loop env))

(define (set-definition-value! var val env)
  (let ((frame (first-frame env)))
    (define (scan pairs)
      (cond ((empty? pairs)
             (add-binding-to-frame var val frame))
            ((eq? (caar pairs) var)
             (set-cdr! (car pairs) val)
             frame)
            (else 
             (scan (cdr pairs)))))
    (set-car! env (add-binding-to-frame var val frame))))
