#lang scheme

; Exercise 1.1.  Below is a sequence of expressions. What is the result printed by
; the interpreter in response to each expression? Assume that the sequence is to
; be evaluated in the order in which it is presented.

10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
; a = 3
(define b (+ a 1))
; b = 4
(+ a b (* a b))
; 3 + 4 + 12 = 19
(= a b)
; false
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a) b a))
; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16

; Exercise 1.2.  Translate the following expression into prefix form

; 5 + 4 + (2 - (3 - (6 + 4/5)))

+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))

; -----------------------------
;   3(6 - 2)(2 - 7)

* 3 (- 6 2) (- 2 7)

; Exercise 1.3.  Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers.

(define (square a) (* a a))

(define (sum-squares a b)
  (+ (square a) (square b)))

; (define (sum-squares-largest ...))
(define (sum-squares-largest a b c)
  (if (> a b) 
      (sum-squares a (if (> b c) b c))
      (sum-squares b (if (> a c) a c))
  )
)

; Exercise 1.4.  Observe that our model of evaluation allows for combinations
; whose operators are compound expressions. Use this observation to describe the
; behavior of the following procedure:
;
;(define (a-plus-abs-b a b)
;  ((if (> b 0) + -) a b))
; 1. evaluate (> b 0) to true or false
; 2. evaluate (if (true or false) + -) to + or -
; 3. evaluate (+ a b) or (- a b)

; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter
; he is faced with is using applicative-order evaluation or normal-order evaluation. He
; defines the following two procedures:

; (define (p) (p))

; (define (test x y)
;   (if (= x 0)
;       0
;       y))

; Then he evaluates the expression

; (test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order
; evaluation? What behavior will he observe with an interpreter that uses
; normal-order evaluation? Explain your answer. (Assume that the evaluation rule
; for the special form if is the same whether the interpreter is using normal or
; applicative order: The predicate expression is evaluated first, and the result
; determines whether to evaluate the consequent or the alternative expression.)

; applicative:
; 1. p is evaluated to p
; 2. p is evaluated to p
; ... endless recursion

; normal:
; 1. check (= x 0)
; 2. return 0

; Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
; ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's
; friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
; 5

(new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))

; What happens when Alyssa attempts to use this to compute square roots? Explain.
; 1. call (sqrt-iter 1 2)
; 2. call (new-if predicate guess (recursive call))
; 3. arguments are evaluated: predicate, guess, recursive call
; 4. ... endless loop

; Exercise 1.7.  The good-enough? test used in computing square roots will not
; be very effective for finding the square roots of very small numbers. Also, in
; real computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the test fails for small and large
; numbers. An alternative strategy for implementing good-enough? is to watch how
; guess changes from one iteration to the next and to stop when the change is a
; very small fraction of the guess. Design a square-root procedure that uses
; this kind of end test. Does this work better for small and large numbers?
;
; small number: sqrt-iter 0.0001 0.00005
; 0.0001 / 0.00005 = 2
; (0.0001 + 2) / 2 = 2.0001 / 2 = 1.00005
; sqrt-iter 0.0001 1.00005
; 0.0001 / 1.00005 = 0.000099995
; (0.0001 + 0.000099995) / 2 = 0.0000999975
; sqrt-iter 0.0001 0.0000999975
; 0.0001 / 0.0000999975 = 1.00002500063
; (0.0001 + 1.00002500063) / 2 = 0.50006250031

;Exercise 1.8.  Newton's method for cube roots is based on the fact that if y 
;is an approximation to the cube root of x, then a better approximation is given 
;by the value

;Use this formula to implement a cube-root procedure analogous to the square-root 
;procedure. (In section 1.3.4 we will see how to implement Newton's method in 
;general as an abstraction of these square-root and cube-root procedures.)

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))
