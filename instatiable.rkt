#lang racket

(define (put employee division) '())
(define (get employee division) '())

(define (make-file division file)
  (cons division file))
(define (division-in-file from-bulk)
  (car from-bulk))
(define (raw-file from-bulk)
  (cdr from-bulk))

(define (make-record division record)
  (cons division record))
(define (division-in-record from-bulk)
  (car from-bulk))
(define (raw-record from-bulk)
  (cdr from-bulk))

(define (get-record-division1 from-employee-name) '())
(define (get-record-division2 from-employee-name) '())
(define (get-salary-division1 from-employee-name) '())
(define (get-salary-division2 from-employee-name) '())

(define (install-all)
  (put 'get-record 'division1 get-record-division1)
  (put 'get-record 'division2 get-record-division2)
  (put 'get-salary 'division1 get-salary-division1)
  (put 'get-salary 'division2 get-salary-division2))

(define (get-record for-employee from-file)
  ((get 'get-record (division-in-file from-file) for-employee)))

(define (get-salary for-employee from-record)
  ((get 'get-salary (division-in-record from-record)) for-employee))

(define (find-employee-record employee-name files)
  (filter (lambda(file) 
            (not (null? (get-record employee-name file)))) 
          files))
