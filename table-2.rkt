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

(define (lookup k1 k2 table)
  (let ((subtable (assoc k1 (records table))))
    (if subtable 
        (let ((record (assoc k2 (records subtable))))
          (if record
              (value record)
              false))
        false)))
    
(define (insert-table! k1 k2 v table)
    (let ((found-subtable (assoc k1 (records table))))
      (if found-subtable
          (let ((found-pair (assoc k2 (records found-subtable))))
            (if found-pair
                (set-value! found-pair 
                            v)
                (set-records! found-subtable 
                              (add-record (make-pair k2 v) 
                                          (records found-subtable)))))
          (let ((new-records (mlist (make-pair k2 v))))
            (set-records! table 
                          (add-record (make-pair k1 new-records) 
                                      (records table)))))))

(define (make-table)
  (mlist '*table*))

(define t 
  ((lambda ()
     (let ((new-table (make-table)))
       (insert-table! 'letters 'a 97 new-table)
       new-table))))

