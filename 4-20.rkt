#lang racket
;(define (evaluator-eval exp env)
;  (cond ((self-evaluating? exp) exp)
;....
;        ((letrec? exp)
;         (evaluator-eval (letrec->combination exp) env))
;....        
;        (else 
;         (error "Unknown expression type -- EVAL" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (let-rec? exp) 
  (tagged-list? exp 'let-rec))

(define (make-let arguments body)
  (cons 'let (cons arguments body)))

(define (get-let-rec-args exp)
  (cadr exp))

(define (get-let-rec-body exp)
  (cddr exp))

(define (make-let-items args)
  (define (get-var pair) (car pair))
  (define (get-exp pair) (cadr pair))
  (define (make-unassigned var) (list var '*unassigned*))
  (define (make-assignement var exp) (list 'set! var exp))
  (define (iter-split rest unassigned assignement)
    (if (empty? rest)
        (list unassigned assignement)
        (let ((pair (car rest)))
          (let((var (get-var pair)) (exp (get-exp pair)))
            (iter-split (cdr rest)
                        (cons (make-unassigned var) unassigned)
                        (cons (make-assignement var exp) assignement))))))
  (iter-split args '() '()))
  
(define (letrec->combination exp)
  (let ((let-rec-args (get-let-rec-args exp))
        (let-rec-body (get-let-rec-body exp)))
    (let ((let-items (make-let-items let-rec-args)))
  (make-let (car let-items) (append  (cadr let-items) let-rec-body)))))

(define a-let-exp '(letrec ((x 1) (y 2)) (+ x y)))