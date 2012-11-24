#lang planet neil/sicp
;;Ex 4-2
;;If one want to use application (call + 2 3)
;;(define (application? exp) (tagged-list? exp 'call))
;;(define (operator exp) (cadr exp))
;;(define (operands exp) (cddr exp))

(define (evaluator-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted-list? exp) 
         (let ((text (quoted->list exp)))
           (evaluator-eval  text env)))
        ((quoted? exp) (text-of-quotation exp))
        ((assignement? exp) (eval-assignement exp env))
        ((definition? exp) (eval-definition exp env))
        ((undefine? exp) (eval-undefine exp env))
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
         (evaluator-apply (actual-value (operator exp) env)
                          (operands exp)
                          env))
        (else 
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (evaluator-eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp obj)
  (cadr obj))

(define (thunk-env obj)
  (caddr obj))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) 
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk )
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (evaluator-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure 
                                    (list-of-args-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-args-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-args-values (cdr exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (cdr exps) env))))

;;Evaluated left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-op (evaluator-eval (first-operand exps) env)))
        (cons first-op
              (list-of-values (rest-operands exps) env)))))

(define (application? exp) (pair? exp))

;;Right to left
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((rest (list-of-values (rest-operands exps) env)))
;        cons (eval (first-operand exp) env)
;              rest)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (evaluator-eval (if-consequent exp) env)
      (evaluator-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (evaluator-eval (first-exp exps) env))
        (else 
         (evaluator-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignement exp env)
  (set-variable-value! (assignement-variable exp)
                       (evaluator-eval (assignement-value exp) env)
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

(define (quoted-list? exp)
  (and (quoted? exp)
       (pair? (cadr exp))))

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
                   (cddr exp))))

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

(define (first-exp exps) (car exps))

(define (rest-exps exps) (cdr exps))

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
         (cddr clause))
        (else 
         (cdr clause))))

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

(define (quoted->list exps)
  (define (make-list expr)
    (if (empty? expr)
        (list 'quote '())
        (list 'cons 
              (list 'quote (car expr))
              (make-list (cdr expr)))))
  (make-list (cadr exps)))
  
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

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

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
         (error "missing variable names -- XTEND-ENV" vars args))
        (else 
         (error "missing arguments -- XTEND-ENV" vars args))))

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
    (env-loop callback update var (enclosing-environment e)))
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
        (list 'empty? empty?)
        (list '+ +)
        (list '/ +)
        (list '= =)
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



;;is unbind possible... Ex 4-13

(define (undefine? exp) (tagged-list? exp 'undefine))

(define (eval-undefine exp env)
  (unset-definition-value! (definition-variable exp) env))

(define (unset-definition-value! var env)
  (define (unbind vars vals)
    (cond ((null? vars) 
           '())
          ((eq? (car vars) var)
           (let ((val (car vals)))
             (set-car! vars '())
             (set-car! vals '())
             val))
          (else
           (unbind (cdr vars) (cdr vals)))))
      (let ((frame (first-frame env)))
        (let ((vars (frame-variables frame))
              (vals (frame-values frame)))
        (unbind vars vals))))

(define input-prompt ";; L-eval input:")
(define output-prompt ";; L-eval output:")

(define (driver-loop)
  (print input-prompt)
  (let ((input (read)))
    ;(print global-env)
    (let ((evaluated (actual-value input global-env)))
      (user-print evaluated)
      (driver-loop))))

(define (print string)
  (newline)
  (display string)
  (newline))
  
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
