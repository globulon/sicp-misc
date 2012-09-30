#lang scheme
(require racket/mpair)

(define (mcaar ml)
  (mcar (mcar ml)))

(define (records table)
  (mcdr table))

(define (add-record pair records)
  (mcons pair records))

(define (first-pair recs)
  (mcar recs))

(define (make-pair k v)
  (mcons k v))

(define (head table)
  (mcar table))

(define (value pair)
  (mcdr pair))

(define (key pair)
  (mcar pair))

(define (set-value! pair val)
  (set-mcdr! pair val))

(define (set-records! table recs)
  (set-mcdr! table recs))

(define (rest records)
  (mcdr records))
