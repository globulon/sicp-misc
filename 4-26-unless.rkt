#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (unless? exp) 
  (tagged-list? exp 'unless))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (unless->if exp)
  (let ((condition (cadr exp))
        (consequent (cadddr exp))
        (alternate (caddr exp)))
    (make-if condition consequent alternate)))