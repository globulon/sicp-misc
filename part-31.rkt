#lang racket
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds")))

(define (call-the-cops value) "I called the cops")

(define (authorize passwd)
  (define invalid (make-monitored (lambda (value) "incorrect password")))
  (define (authorized f secret)
    (cond ((eq? secret passwd) f)
          ((eq? 3 (invalid 'how-many-calls?)) call-the-cops)
          (else invalid)))
  authorized)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (join other-password)
      (secured-dispatch (authorize password)))
  (define (secured-dispatch authorized)
    (define (dispatch m secret)
      (cond ((eq? m 'withdraw) (authorized withdraw secret))
            ((eq? m 'deposit) (authorized deposit secret))
            ((eq? m 'join) (authorized join secret))
            (else (error "Unknown request --MAKE ACCOUNT--" m))))
    dispatch)
  (secured-dispatch (authorize password)))

(define (make-joint account main-password other-password)
  ((account 'join main-password) other-password))

(define (make-accumulator value)
  (let ((current value))
    (lambda (delta)
        (set! current (+ delta current))
        current)))


(define (make-monitored f)
  (let ((accumulated 0))
    (define (invoke fct value)
      (set! accumulated (+ 1 accumulated))
      (fct value))
    (define (dispatch value)
      (if (eq? value 'how-many-calls?) 
          accumulated
          (invoke f value)))
    dispatch))

(define (lcg a c m)
  (lambda (x)
    (modulo (+ (* a x) c) m)))

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define rand
  (let ((x random-init))
    (define reset
      (lambda (new-value)
        (set! x new-value)
        x))
    (define (update)
      (set! x (rand-update x))
      x)
    (define (dispatch m)
      (cond ((eq? m 'reset) reset)
            ((eq? m 'generate) (update))
            (else (error "method does not exists" m))))
    dispatch))
           
(define (estimate-pi trials)        
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= 1 (gcd (rand 'generate) (rand 'generate))))

(define (monte-carlo trials experiment)
  (define (iter remaining-trials trials-passed)
     (cond ((= remaining-trials 0) (* 1.0 (/ trials-passed trials)))
           ((experiment) (iter (- remaining-trials 1) (+ trials-passed 1)))
           (else (iter (- remaining-trials 1) trials-passed))))
  (iter trials 0))

(define (square x)
  (* x x))

(define (estimate-integral x1 x2 y1 y2 trials)
  (define (middle x1 x2) (/ (+ x1 x2) 2.0))
  (define (rect-area x1 x2 y1 y2) (* (- x2 x1) (- y2 y1)))
  (define (rad x1 x2) (- x2 (middle x1 x2)))
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))          
  (define (in-circle cx cy radius)
    (let ((rr (square radius)))
      (lambda (x y)
        (let ((xx (square (- x cx)))
              (yy (square (- y cy))))
          (<= (+ xx yy) rr)))))
  (define (find is-in-circle)
    (lambda ()
      (is-in-circle (random-in-range x1 x2) (random-in-range y1 y2))))
  (let ((is-in-circle (in-circle (middle x1 x2) (middle y1 y2) (rad x1 x2))))
    (let ((experiment (find is-in-circle)))
      (* (rect-area x1 x2 y1 y2)(monte-carlo trials experiment)))))


