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

(define (assoc k records)
  (cond ((empty? records) 
         false)
        ((equal? k (key (first-pair records)))
         (first-pair records))
        (else
         (assoc k (rest records)))))

(define (lookup k table)
  (let ((record (assoc k (records table))))
    (if record
        (value record)
        false)))

(define (insert-table! k v table)
    (let ((found-pair (assoc k (records table))))
      (if found-pair
          (set-value! found-pair 
                      v)
          (set-records! table 
                        (add-record (make-pair k v) 
                                    (records table))))))


(define (make-table)
  (mlist '*table*))

(define t 
  ((lambda ()
     (let ((new-table (make-table)))
       (insert-table! 'a 1 new-table)
       new-table))))

