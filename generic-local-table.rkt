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

(define (assoc-single k records)
  (cond ((not (mpair? records))
         false)
        ((empty? records) 
         false)
        ((equal? k (key (first-pair records)))
         (first-pair records))
        (else
         (assoc-single k (rest records)))))

(define (assoc keys records)
  (let ((first-key (car keys))
        (rest (cdr keys)))
    (let ((found-pair (assoc-single first-key records)))
      (cond ((and (empty? rest) found-pair)
             found-pair)
            ((and found-pair (mpair? found-pair))
             (assoc rest (value found-pair)))
            (else 
             false)))))
    
(define (lookup keys table)
  (if (empty? keys)
      false
      (let ((record (assoc keys (records table))))
        (if record
            (value record)
            false))))

(define (insert-table! keys v table)
  (let ((key (car keys)) (rest (cdr keys)))
    (let ((found-pair (assoc-single key (records table))))
      (cond ((and found-pair (empty? rest))
             (display "**1**")
             (set-value! found-pair v))
            ((and found-pair (mpair? (value found-pair)))
             (display key)
             (display found-pair)
             (display "**2**")
             (insert-table! rest v found-pair))
            (found-pair 
             (display "**3**")
             (set-mcdr! found-pair (mlist))
             (insert-table! rest v found-pair))
            ((empty? rest)
             (display key)
             (display found-pair)
             (display "**4**")
             (set-records! table 
                          (add-record (make-pair key v) 
                                      (records table))))
            (else
             (display "**5**")
             (let ((new-sub-table (make-sub-table key)))
               (set-records! table 
                             (add-record new-sub-table 
                                         (records table)))
               (insert-table! rest v new-sub-table)))))))

(define (make-sub-table head)
  (mlist head))


(define (make-table)
  (make-sub-table '*table*))

(define t 
  ((lambda ()
     (let ((new-table (make-table)))
       (insert-table! '(a) 1 new-table)
       new-table))))

