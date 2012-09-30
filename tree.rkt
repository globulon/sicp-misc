#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (make-leaf value) (make-tree value '() '()))

(define (element-of-set? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree)) (element-of-set? 
                             x (left-branch tree)))
        ((> x (entry tree)) (element-of-set? 
                             x (right-branch tree)))))



(define (adjoin-set x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (make-tree (entry tree) 
                                       (adjoin-set x (left-branch tree))
                                       (right-branch tree)))
        ((> x (entry tree)) (make-tree (entry tree)
                                       (left-branch tree)
                                       (adjoin-set x (right-branch tree))))))

(define tree-one
  (make-tree 5 
             (make-tree 3
                        (make-leaf 2)
                        (make-leaf 4))
             (make-tree 6
                        (make-leaf 7)
                        (make-leaf 8))))

(define tree-two
  (make-tree 7 
             (make-tree 3
                        (make-leaf 1)
                        (make-leaf 5))
             (make-tree 9
                        '()
                        (make-leaf 11))))

(define tree-three
  (make-tree 3 
             (make-leaf 1)
             (make-tree 7
                        (make-leaf 5)
                        (make-tree 9
                                   '()
                                   (make-leaf 11)))))

(define tree-four
  (make-tree 5
             (make-tree 3
                        (make-leaf 1)
                        '())
             (make-tree 9
                        (make-leaf 7)
                        (make-leaf 11))))

(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left-branch tree))
              (cons (entry tree) 
                    (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= 0 n)
      (cons '() elements)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elements left-size)))
          (let ((left-tree (car left-result))
                (not-left-tree (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car not-left-tree))
                  (right-result (partial-tree (cdr not-left-tree) right-size)))
              (let ((right-tree (car right-result))
                    (remainings (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remainings))))))))

(define (intersection-ordered-set s1 s2)
  (if (or (null? s1) (null? s2)) 
      '()
      (let ((first1 (car s1)) 
            (first2 (car s2)))
        (cond ((= first1 first2) 
               (cons first1 
                     (intersection-ordered-set (cdr s1) 
                                               (cdr s2))))
              ((< first1 first2) 
               (intersection-ordered-set (cdr s1) s2))
              ((> first1 first2) 
               (intersection-ordered-set s1 (cdr s2)))))))

(define (union-ordered-set s1 s2)
  (define (union-rest item tail1 tail2)
    (cons item (union-ordered-set tail1 tail2)))
  (cond ((null? s1) s2)
        ((null? s2) s1) 
        (else (let ((first1 (car s1)) 
                    (first2 (car s2)))
                (cond ((= first1 first2) 
                       (union-rest first1 (cdr s1) (cdr s2)))
                      ((< first1 first2) 
                       (union-rest first1 (cdr s1) s2))
                      ((> first1 first2) 
                       (union-rest first2 s1 (cdr s2))))))))

(define (intersection-tree t1 t2)
  (let((s1 (tree->list2 t1)) 
       (s2 (tree->list2 t2)))
    (list->tree (intersection-ordered-set s1 s2))))

(define (union-tree t1 t2)
  (let((s1 (tree->list2 t1)) 
       (s2 (tree->list2 t2)))
    (list->tree (union-ordered-set s1 s2))))

(define (make-record key data)
  (list key data))

(define (key record)
  (car record))

(define (data record)
  (cadr record))

(define some-records
  (make-tree (make-record 5 'a) 
             (make-tree (make-record 2 'b) 
                        (make-leaf (make-record 3 'c) )
                        (make-leaf (make-record 4 'd) ))
             (make-tree (make-record 6 'e) 
                        (make-leaf (make-record 7 'f) )
                        (make-leaf (make-record 5 'g) ))))

(define (lookup given-key records)
  (if (null? records) 
      #f
      (let ((entry-key (key (entry records))))
        (cond ((= given-key entry-key) (entry records))
              ((< given-key entry-key) (lookup given-key (left-branch records)))
              ((> given-key entry-key) (lookup given-key (right-branch records)))))))

