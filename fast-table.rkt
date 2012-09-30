#lang scheme
(require racket/mpair)

(define (make-tree entry left right)
  (mlist entry left right))

(define (left-branch tree)
  (mcar (mcdr tree)))

(define (right-branch tree)
  (mcar (mcdr (mcdr tree))))

(define (entry tree)
  (mcar tree))

(define (make-element-of-set identical? 
                             lower-than? 
                             greater-than?)
  (define (element-of-set x tree)
    (cond ((null? tree)
         #f)
        ((identical? x (entry tree))
         (entry tree))
        ((lower-than? x (entry tree))
         (element-of-set x (left-branch tree)))
        ((greater-than? x (entry tree))
         (element-of-set x (right-branch tree)))))
  element-of-set)

(define (make-adjoint-set  identical? 
                           lower-than? 
                           greater-than?)
  (define (adjoint-set x set)
    (cond ((null? set)
           (make-tree x '() '()))
          ((identical? x (entry set))
           set)
          ((lower-than? x (entry set))
           (make-tree (entry set) 
                      (adjoint-set x (left-branch set))
                      (right-branch set)))
          ((greater-than? x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoint-set x (right-branch set))))))
  adjoint-set)
  
(define (make-pair k v)
  (mcons k v))

(define (value pair)
  (mcdr pair))

(define (key pair)
  (mcar pair))

(define (set-value! pair val)
  (set-mcdr! pair val))

(define (set-records! table recs)
  (set-mcdr! table recs))

(define (records table)
  (mcdr table))

(define (same-key? k p)
  (= k (key p)))

(define (lower-key? k p)
  (< k (key p)))

(define (greater-key? k p)
  (> k (key p)))

(define (same-pair? p1 p2)
  (= (key p1) (key p2)))

(define (lower-pair? p1 p2)
  (< (key p1) (key p2)))

(define (greater-pair? p1 p2)
  (> (key p1) (key p2)))

(define element-of-tree
  (make-element-of-set same-key?
                       lower-key?
                       greater-key?))
(define adjoint-tree
  (make-adjoint-set same-pair?
                    lower-pair?
                    greater-pair?))

(define (assoc k tree)
  (element-of-tree k tree))

(define (lookup k table)
  (let ((found (assoc k (records table))))
    (if found
        (value (assoc k (records table)))
        #f)))

(define (insert-table! k v table)
  (let ((pair (assoc k (records table))))
    (if pair
        (set-value! pair v)
        (let ((new-pair (make-pair k v)))
          (set-records! table (adjoint-tree new-pair (records table)))))))
    
(define (make-table)
  (mlist "**table**"))

(define t 
  ((lambda ()
    (let ((table (make-table)))
      (insert-table! 1 'a table)
      table))))