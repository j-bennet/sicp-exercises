#lang scheme

; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4
;
; ex:
;
; 2.46
; 2.47 (just one implementation is enough)
; 2.48
; 2.49 (feel free to not do "d")
; 2.50
; 2.51
; 2.44 (optional). Apparently you can ONLY do 2.44 and 2.45 after you did all the above.

; Exercise 2.46.  A two-dimensional vector v running from the origin to a point can be
; represented as a pair consisting of an x-coordinate and a y-coordinate. Implement
; a data abstraction for vectors by giving a constructor make-vect and corresponding
; selectors xcor-vect and ycor-vect. In terms of your selectors and constructor,
; implement procedures add-vect, sub-vect, and scale-vect that perform the operations
; vector addition, vector subtraction, and multiplying a vector by a scalar.

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Exercise 2.47. A constructor for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; Supply the appropriate selectors to produce an implementation for frames.
(define (origin-frame frame)
  (list-ref frame 0))

(define (edge1-frame frame)
  (list-ref frame 1))

(define (edge2-frame frame)
  (list-ref frame 2))

; Exercise 2.48.  A directed line segment in the plane can be represented as a pair
; of vectors -- the vector running from the origin to the start-point of the segment,
; and the vector running from the origin to the end-point of the segment. Use your
; vector representation from exercise 2.46 to define a representation for segments
; with a constructor make-segment and selectors start-segment and end-segment.

(define (make-segment a b)
  (cons a b))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

; Exercise 2.49.  Use segments->painter to define the following primitive painters:
;

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (draw-line x y)
  (printf "line from ~v to ~v" x y)
  (newline))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;
; a.  The painter that draws the outline of the designated frame.
;
(define outline-segments
  (list 
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 1 1))))

(define outline-painter (segments->painter outline-segments))

; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
;
(define x-segments
  (list
    (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))))

(define x-painter (segments->painter x-segments))

; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;
(define diamond-segments
  (list
    (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))))

(define diamond-painter (segments->painter diamond-segments))

; d.  The wave painter.
(define wave 
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         (make-segment (make-vect 0.50 0.70) (make-vect 0.35 0.75)) ;smile 1
         (make-segment (make-vect 0.50 0.70) (make-vect 0.65 0.75)) ;smile 2
         )))

; Exercise 2.50.  Define the transformation flip-horiz, which flips painters
; horizontally, and transformations that rotate painters counterclockwise
; by 180 degrees and 270 degrees.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (rotate90 painter)
  (transform-painter 
    painter
    (make-vect 0.0 1.0)     ; new origin
    (make-vect 0.0 0.0)     ; new end of edge1
    (make-vect 1.0 1.0)))   ; new end of edge2

(define (rotate180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)   ; new origin
    (make-vect 0.0 0.0)   ; new end of edge 1
    (make-vect 1.0 1.0))) ; new end of edge 2

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


