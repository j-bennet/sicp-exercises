#lang scheme

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than
; the method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as
; http://en.wikipedia.org/wiki/Simpson%27s_rule
; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing
; n increases the accuracy of the approximation.) Define a procedure that takes
; as arguments f, a, b, and n and returns the value of the integral, computed using
; Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100
; and n = 1000), and compare the results to those of the integral procedure shown above.

(define (simpson f a b n)
  (let* ([h (/ (- b a) n)]
         [s1 (+ (f a) (f b))])
    
    (define (add2 x) (+ 2 x))
    (define (term2 k) (* 2 (f (+ a (* k h)))))
    (define (term4 k) (* 4 (f (+ a (* k h)))))
    
    (let* ([s2 (sum term4 1 add2 n)]
           [s3 (sum term2 2 add2 (- n 1))])
      (/ (* h (+ s1 s2 s3)) 3))))

; Exercise 1.30.  The sum procedure above generates a linear recursion. The procedure 
; can be rewritten so that the sum is performed iteratively. Show how to do this by
; filling in the missing expressions in the following definition:

(define (sumi term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Exercise 1.31.   
; a.  The sum procedure is only the simplest of a vast number of similar abstractions
; that can be captured as higher-order procedures.51 Write an analogous procedure
; called product that returns the product of the values of a function at points over
; a given range. Show how to define factorial in terms of product. Also use product
; to compute approximations to  using the formula52
; b.  If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write one
; that generates a recursive process.

; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; iterative
(define (producti term a next b)
  (define (iter x accum)
    (if (> x b)
        accum
        (iter (next x) (* (term x) accum))))
  (iter a 1))
