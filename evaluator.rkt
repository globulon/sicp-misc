#lang planet neil/sicp

;;Ex 4-2
;;If one want to use application (call + 2 3)
;;(define (application? exp) (tagged-list? exp 'call))
;;(define (operator exp) (cadr exp))
;;(define (operands exp) (cddr exp))

(define (evaluator-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignement? exp) (eval-assignement exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp)
         (evaluator-eval (let->combination exp) env))
        ((let*? exp)
         (evaluator-eval (let*->nested-lets exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp)env))
        ((cond? exp) (evaluator-eval (cond->if exp) env))
;ex 4-3        
        ((and? exp) (evaluator-eval (and->if exp) env))
        ((or? exp) (evaluator-eval (or->if exp) env))
        ((do? exp) (evaluator-eval (do->begin exp) env))
;        ((and? exp)
;         (eval-and (logical-operands exp) env))
;        ((or? exp)
;         (eval-or exp (logical-operands exp) env))
        ((application? exp)
         (evaluator-apply (evaluator-eval (operator exp) env)
                          (list-of-values (operands exp) env)))
        (else 
         (error "Unknown expression type -- EVAL" exp))))

(define (evaluator-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))


;;Evaluated left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-op (eval (first-operand exps))))
        cons first-op
              (list-of-values (rest-operands exps) env))))

(define (application? exp) (pair? exp))

;;Right to left
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((rest (list-of-values (rest-operands exps) env)))
;        cons (eval (first-operand exp) env)
;              rest)))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (evaluator-eval (first-exp exps) env))
        (else 
         (evaluator-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignement exp env)
  (set-variable-value! (assignement-variable exp)
                       (eval (assignement-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (set-definition-value! (definition-variable exp)
                         (evaluator-eval (definition-value exp) env)
                         env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #f)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignement? exp) 
  (tagged-list? exp 'set!))

(define (assignement-variable exp)
  (cadr exp))

(define (assignement-value exp)
  (caddr exp))
              
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

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp) 
  (if (not (null? (cadddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (first-exp exps) (car exp))

(define (rest-exps exps) (cdr exp))

(define (last-exp? exp)
  (empty? (rest-exps exp)))

(define (sequence->exp exp)
  (cond ((empty? exp) exp)
        ((last-exp? exp) (first-exp exp))
        (else (make-begin exp))))

(define (make-begin exps)
  (cons 'begin exps))

(define (applications? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (empty? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause) (tagged-list? clause 'else))

(define (clause-predicate clause) (car clause))

(define (clause-actions clause) 
  (cond ((eq? "=>" (cadr clause)) 
         (cddr clause))))

(define (cond->if exp) 
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (empty? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (empty? rest)
                (sequence->exp (clause-actions first))
                (error "Invalid position for else clause expand-clauses"))
            (make-if (clause-predicate first)
                     (sequence->exp (clause-actions first))
                     (expand-clauses rest))))))

(define (logical-operands exp) (cdr exp))

(define (first-logical-operand operands) (car operands))

(define (rest-logical-operands operands) (cdr operands))

(define (no-logical-operand? operands) (empty? operands))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-terms exp) (logical-operands exp))

;;ex 4-3 as adding and/or conditions
(define (evaluate-and exps env)
  (define (iter exps)
    (cond ((no-logical-operand? exps) #t)
          ((not (evaluator-eval (first-logical-operand exp) env)) #f)
          (else (evaluate-and (rest-logical-operands exp) env))))
  (if (no-logical-operand? exps)
      #f
      (iter exps)))

(define (and->if exp)
  (if (no-logical-operand? exp)
      'true
      (make-if (first-logical-operand exp)
               (and->if (rest-logical-operands exp))
               'false)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (evaluate-or exps env)
  (cond ((no-logical-operand? exps) #f)
        ((evaluator-eval (first-logical-operand exps) env) #t)
        (else (evaluate-or (rest-logical-operands exps) env))))

(define (or->if exp)
  (if (no-logical-operand? exp)
      'false
      (make-if (first-logical-operand exp)
               'true
               (or->if (rest-logical-operands exp)))))

;;Ex 4-6 adding let expressions
(define (let? exp) (tagged-list? exp 'let))

(define (make-define variable definition)
  (list 'define variable definition))

(define (lambda-let->combination)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (let ((params (map car bindings))
          (exps (map cadr bindings)))
      (cons
       (make-lambda params body)
       exps))))

;;ex 4-8 : add a named let
(define (named-let->combination exp)
  (let ((name (cadr exp))
        (bindings (caddr exp))
        (body (cdddr exp)))
    (let ((params (map car bindings))
          (exps (map cadr bindings)))
      (make-begin
       (list (make-define
              name
              (make-lambda params body))
             (cons name exps))))))

(define (let->combination exp)
  (if (list? (cadr exp))
      (lambda-let->combination)
      (named-let->combination exp)))

;;ex 4-7 let expressions
(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (rest-assignements assignements) 
    (cdr assignements))
  (define (first-assignement assignements) 
    (car assignements))
  (define (make-let arguments body)
    (cons 'let (cons arguments body)))
  (define (make-rec-let assignements body)
    (cond ((empty? assignements) (make-let assignements body))
          ((empty? (cdr assignements)) (make-let assignements body))
          (else (make-let (list (car assignements)) 
                          (list (make-rec-let (cdr assignements) body))))))
  (let ((assignements (cadr exp)))
    (let ((body (cddr exp)))
         (make-rec-let assignements body))))
     
;;Ex 4-9: add a do
(define (do? exp) (tagged-list? exp 'do))

(define (no-predicates? predicates)
  (empty? predicates))

(define (first-predicate predicates)
  (car predicates))

(define (rest-predicates predicates)
  (cdr predicates))

(define (do->if predicates incs body)
  (if (no-predicates? predicates)
      (make-begin 
       (append body (list (cons 'do-function incs))))
      (list (make-if (first-predicate predicates)
               (do->if (rest-predicates predicates) incs body)
               ''done))))

(define (make-do-lambda args predicates incs body)
  (make-lambda args
               (do->if predicates incs body)))

(define (do->begin exp)
  (let ((variables (cadr exp))
        (predicates (caddr exp))
        (body (cdddr exp)))
    (let ((args (map car variables))
          (inits (map cadr variables))
          (incs (map caddr variables)))
      (make-begin
       (list (make-define 'do-function
                    (make-do-lambda args 
                                    predicates
                                    incs
                                    body))
       (cons 'do-function inits))))))

(define (true? x) (not (eq? false)))

(define (false? x) (eq? false))

(define (apply-primitive-procedure proc args)
  ;;book supposes we have it...we re-pipe it to real apply
  (apply proc args))

;;waiting for better definitions in 4.1.4
(define (primitive-procedure? p) #t)

(define (make-procedure arguments body env)
  (list 'procedure arguments body env))

;;procedures
(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;environment
;;added optimization for ex 4-12
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

(define (make-scan callback action var)
  (define (scan vars vals)
    (cond ((empty? vars) 
           (callback))
          ((eq? (car vars) var) 
           (action vals))
          (else 
           (scan (cdr vars) (cdr vals)))))
  (lambda (frame)
    (let ((vars (frame-variables frame))
          (vals (frame-values frame)))
      (scan vars vals))))

(define (lookup-variable-value var env)
  (define (make-callback e)
    (lambda () 
      (env-loop (enclosing-environment e))))
  (define (env-loop e)
    (if (eq? the-empty-environment e)
        (error "variable not bound -- LOOKUP VARIABLE VALUE" var)
        (let ((frame (first-frame e))
              (callback (make-callback e)))
               ((make-scan callback car var) frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (make-callback e)
    (lambda () 
      (env-loop (enclosing-environment e))))
  (define (update vals)
    (set-car! vals val))
  (define (env-loop e)
    (if (eq? the-empty-environment e)
        (error "variable not bound -- LOOKUP VARIABLE VALUE" var)
        (let ((frame (first-frame e))
              (callback (make-callback e)))
               ((make-scan callback update var) frame))))
  (env-loop env))

(define (set-definition-value! var val env)
  (define (extend-frame frame)
    (lambda () 
      (add-binding-to-frame var val frame)))
  (define (update vals)
    (set-car! vals val))
  (let ((frame (first-frame env)))
    (let (( scan (make-scan (extend-frame frame) update var)))
      (scan frame))))
