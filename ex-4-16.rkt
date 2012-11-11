#lang planet neil/sicp
;;procedures

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
  (define (get-val vals)
    (let ((val (car vals)))
      (if (eq? '*unassigned* val)
          (error "variable not assigned yet --" var)
          val)))
  (define (callback e)
    (env-loop callback get-val var (enclosing-environment e)))
  (env-loop callback get-val var env))

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

(define e (extend-environment '(x y z) '(1 2 3) the-empty-environment))

;(lambda <vars>
;  (define u <e1>)
;  (define v <e2>)
;  <e3>)

;would be transformed into

;(lambda <vars>
;  (let ((u '*unassigned*)
;        (v '*unassigned*))
;    (set! u <e1>)
;    (set! v <e2>)
;    <e3>))

(define (tagged-list? exp value)
  (if (pair? exp)
      (eq? value (car exp))
      #f))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cdddr exp))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let arguments body)
    (cons 'let (cons arguments body)))

(define (scan-out-defines body)
  (define (make-unassigned var)
    (list var '*unassigned*))
  (define (make-set var value)
    (list 'set! var value))
  (define (rest-exps body) (cdr body))
  (define (first-exp body) (car body))
  (define (scan body unassigned assigned)
    (let ((exp (first-exp body)))
      (cond ((definition? exp)
             (let ((var (definition-variable exp))
                   (val (definition-value exp)))
               (scan (rest-exps body) 
                     (cons (make-unassigned var) unassigned)
                     (cons (make-set var val) assigned))))
             (else
             (make-let unassigned 
                       assigned)))))
  (scan body '() '()))

(define (make-procedure arguments body env)
  (list 'procedure arguments (scan-out-defines body) env))
 ;;I would not use it in the procedure-body
  ;; it would force the conversion for each application
  ;;with make-procedure it would be built on procedure creation only


(define body '((define a 1) (define b 2) (+ a b)))