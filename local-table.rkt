#lang scheme
(require racket/mpair)

(define (make-table is-equal?)
  (let ((a-table (mlist '*table*)))
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
                ((is-equal? k (key (first-pair records)))
                 (first-pair records))
                (else
                 (assoc k (rest records)))))
        (define (lookup k)
          (let ((record (assoc k (records a-table))))
            (if record
                (value record)
                false)))
        (define (insert-table! k v)
          (let ((found-pair (assoc k (records a-table))))
            (if found-pair
                (set-value! found-pair 
                            v)
                (set-records! a-table 
                              (add-record (make-pair k v) 
                                          (records a-table))))))
    (define (dispatch m)
      (cond ((equal? m 'insert)
             insert-table!)
            ((equal? m 'lookup)
             lookup)
            (else 
             (error "unknown method on table"))))
  dispatch))
        
        
(define table 
  ((lambda ()
     (let ((table (make-table equal?)))
       ((table 'insert) 'a 1)
       table))))

