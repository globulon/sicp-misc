(define (identity painter) painter)

(define (transform-painter painter origin corner-one corner-two)
  (lambda (frame)
    (let ((m (frame-coord-map frame))) 
      (let ((new-origin (m origin)))
        (painter 
         (make-frame new-origin
                     (sub-vect (m corner-one) new-origin)
                     (sub-vect (m corner-two) new-origin)))))))

(define (flip-vertical painter)
  (transform-painter painter 
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (beside painter1 painter2) 
  (let ((left-panel 
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 0.5 0.0)
                            (make-vect 0.0 1.0)))
        (right-panel 
         (transform-painter painter2
                            (make-vect 0.5 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 1.0))))
  (lambda (frame) 
    (left-panel frame)
    (right-panel frame))))

(define (below painter) '())

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter))) 
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (split frame sub-frames)
  (lambda (painter n)
    (i (= 0 n)
       painter 
       (let ((smaller ((split frame sub-frames) painter (- n 1))))
         (frame painter (sub-frames smaller smaller))))))

(define (flipped-pairs painter)
  (let ((combined (square-of-four 
                   identity 
                   flip-vert 
                   identity 
                   flip-vert)))
        (combined painter)))

(define right-split(split beside below))

(define up-split (split below beside))

(define (corner-split painter n)
  (let ((up (up-slip painter (- n 1)))
        (right (right-split painter (- n 1))))
    (let ((top-left (beside up up))
          (bottom-right (below right right)))
      (beside (below painter top-left) (below bottom-right corner)))))

(define (square-limit painter n)
  (let ((combined (square-of-four flip-horiz identity rotate180)))
    (combined (corner-split painter n))))

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))

(define (add-vect vector-one vector-two)
  (op-vect + vector-one vector-two))

(define (sub-vect vector-one vector-two)
  (op-vect - vector-one vector-two))

(define (op-vect operator vector-one vector-two)
  (make-vect   (operator (xcor-vect vector-one) 
                  (xcor-vect vector-two))
               (operator (ycor-vect vector-one)
                  (ycor-vect vector-two))))

(define (scale-vect a vector)
  (make-vect (* a (xcor-vect vector)) 
               (* a (ycor-vect vector))))

(define (make-frame origine edge-one edge-two)
  (list origine edge-one edge-two))

(define (origin-frame frame)
  (car frame))

(define (edge-one frame)
  (cadr frame))

(define (edge-two frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v) 
    (add-vect 
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) 
                           (edge-one frame))
               (scale-vect (ycor-vect v)
                           (edge-two frame))))))

(define (make-segment start-segment end-segment)
  (list start-segment end-segment))

(define (start-segment from-segment)
  (car from-segment))

(define (end-segment from-segment)
  (cadr from-segment))

(define (foreach  operation items)
  (define (iter remaining result)
    (if (null? remaining) 
        result
        (iter (cdr remaining) (operation (car remaining)))))
  (iter items '()))

(define (segments->painter segment-list)
  (lambda (frame)
    (let ((mapped (frame-coord-map frame)))
      (foreach 
       (lambda (segment) 
         (draw-line (mapped (start-segment segment))
                    (mapped (end-segment segment))))
       segment-list))))

(define (outline frame)
    (let ((bottom (make-segment (make-vect 0 0) (make-vect 1 0)))
          (right (make-segment (make-vect 1 0) (make-vect 1 1)))
          (top (make-segment (make-vect 1 1) (make-vect 0 1)))
          (left (make-segment (make-vect 0 1) (make-vect 0 0))))
           ((segments->painter (list bottom right top left)) frame)))

(define (draw-x frame)
  (let ((top-left-to-bottom-right
         (make-segment (make-vect 1 0)
                       (make-vect 0 1)))
        (bottom-left-to-top-right
         (make-segment (make-vect 0 0)
                       (make-vect 1 1))))
    ((segments->painter 
      (list top-left-to-bottom-right bottom-left-to-top-right)) 
     frame)))

(define (diamond frame)
  (let ((bottom (make-vect 0.5 0))
        (right (make-vect 1 0.5))
        (top (make-vect 0.5 1))
        (left (make-vect 0 0.5)))
    (segments->painter (list (make-segment bottom right)
                             (make-segment right top)
                             (make-segment top left)
                             (make-segment left bottom)))))


