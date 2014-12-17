#lang scheme

(define (square x) (* x x))

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

; Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases
; of a still more general notion called accumulate that combines a collection of
; terms, using some general accumulation function:
; (accumulate combiner null-value term a next b)
; Accumulate takes as arguments the same term and range specifications as sum
; and product, together with a combiner procedure (of two arguments) that
; specifies how the current term is to be combined with the accumulation of
; the preceding terms and a null-value that specifies what base value to use
; when the terms run out. Write accumulate and show how sum and product
; can both be defined as simple calls to accumulate.

; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

; b. If your accumulate procedure generates a recursive process, write one
; that generates an iterative process. If it generates an iterative process,
; write one that generates a recursive process.
; iterative

(define (accumulatei combiner null-value term a next b)
  (define (iter x accum)
    (if (> x b)
        accum
        (iter (next x) (combiner (term x) accum))))
  (iter a null-value))

; Exercise 1.33.  You can obtain an even more general version of accumulate
; (exercise 1.32) by introducing the notion of a filter on the terms to be combined.
; That is, combine only those terms derived from values in the range that satisfy
; a specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.

(define (filtered-accumulate combiner null-value term a next b filter-cond)
  (define (iter x accum)
    (if (> x b)
        accum
        (if (filter-cond (term x))
            (iter (next x) (combiner (term x) accum))
            (iter (next x) accum))))
  (iter a null-value))

; Show how to express the following using filtered-accumulate:
; a. the sum of the squares of the prime numbers in the interval a to b (assuming
; that you have a prime? predicate already written)
;
; (filtered-accumulate + 0 square 1 inc 5 prime?)
;
; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
;
; (filtered-accumulate * 1 square 1 inc 5 relatively-prime?)
;

;Exercise 1.34.  Suppose we define the procedure

(define (f g)
  (g 2))

;Then we have
;(f square)
;4
;(f (lambda (z) (* z (+ z 1))))
;6
; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)?
; Explain.
;
; (it breaks)
