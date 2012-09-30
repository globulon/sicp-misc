#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-leaf-from-pair pair)
  (make-leaf (car pair) (cadr pair)))


(define (leaf? node)
  (eq? 'leaf (car node)))

(define (symbol-leaf node) (cadr node))
  
(define (weight-leaf node) (caddr node))
 
(define (make-code-tree left right)
  (list left
        right 
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols node)
  (cond ((null? node) '())
        ((leaf? node) (list (symbol-leaf node)))
        ( else (caddr node))))

 (define (weight node)
   (cond ((null? node) 0)
         ((leaf? node) (weight-leaf node))
         ( else (cadddr node))))
 
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (next-branch-from remaining-bits current-branch)
  (let ((bit (car remaining-bits)))
    (cond ((= 0 bit) (left-branch current-branch))
          ((= 1 bit) (right-branch current-branch))
          ( else (error "invalid choice")))))


(define (decode bits tree)
  (define (decode-with-trace remaining-bits current-branch message)
    (if (null? remaining-bits)
        message
        (let ((next-branch (next-branch-from remaining-bits current-branch)))
          (if (leaf? next-branch)
              (decode-with-trace (cdr remaining-bits) 
                                 tree 
                                 (cons (symbol-leaf next-branch) message))
              (decode-with-trace (cdr remaining-bits) 
                                 next-branch
                                 message)))))
  (reverse (decode-with-trace bits tree '()))) 
 
(define sample-tree 
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (symbol-in-tree? value branch)
  (if (leaf? branch)
      (eq? value (symbol-leaf branch))
      (not (null? (filter (lambda (x) (eq? x value)) 
                          (symbols branch))))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-recur branch)
    (cond ((leaf? branch) '())
          ((symbol-in-tree? symbol (left-branch branch)) (cons 0 (encode-symbol-recur (left-branch branch))))
          ((symbol-in-tree? symbol (right-branch branch)) (cons 1 (encode-symbol-recur (right-branch branch))))))
  (if (not (symbol-in-tree? symbol tree))
      (error "symbol not found")
      (encode-symbol-recur tree)))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (filter-pair item assert)
  (let ((symbol (car item)) (value (cadr item)))
    (lambda (pair) 
      (let ((pair-symbol (car pair)) (pair-value (cadr pair)))
        (and (assert pair-value value) (not (eq? symbol pair-symbol)))))))

(define (lower-than item) (filter-pair item <))
      
(define (greater-than item) (filter-pair item >=))

(define (make-leaf-set pairs)
  (if  (null? pairs)
       '()
       (let ((first (car pairs)))
         (append (make-leaf-set (filter (lower-than first) pairs))
                 (cons (make-leaf-from-pair first) 
                       (make-leaf-set (filter (greater-than first) pairs)))))))

(define (successive-merge pairs)
  (let ((size (length pairs)))
    (cond ((= 1 size) pairs)
          ((= 2 size) (make-code-tree (cadr pairs) 
                                      (car pairs)))
          ( else 
           (let ((first (car pairs)) (second (cadr pairs)) (third (caddr pairs)))
             (if (>= (weight second) (weight first))
                 (successive-merge (cons (make-code-tree second first) (cddr pairs)))
                 (successive-merge (cons first (cons (make-code-tree third second) (cdddr pairs))))))))))
                       

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define song-symbols '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(display (length (encode song (generate-huffman-tree song-symbols))))