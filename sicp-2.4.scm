#lang scheme

; Reading: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_sec_2.4

; Selected exercises:

; 2.73
; 2.75
; 2.76 -- this one will make for a good discussion on interface design!

; Exercise 2.73.  Section 2.3.2 described a program that performs
; symbolic differentiation:

; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type
; of the expression to be differentiated. In this situation the
; ``type tag'' of the datum is the algebraic operator symbol
; (such as +) and the operation being performed is deriv.
; We can transform this program into data-directed style by
; rewriting the basic derivative procedure as

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


; Table-related operations are stolen from:
; http://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on
(define global-array '())

(define (make-entry k v) (list k v))

(define (key entry) (car entry))

(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; End of table-related operations

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a.  Explain what was done above. Why can't we assimilate the
; predicates number? and same-variable? into the data-directed dispatch?

; Answer:
; We're going to define and map derivative procedures for each
; possible operator. The generic procedure will retrieve them
; from the table by operator which is the key, and apply to 
; operands.
; We can't assimilate number? and same-variable? because if expression
; is a number or variable, we cannot apply procedure "operator" to it.


; b.  Write the procedures for derivatives of sums and products,
; and the auxiliary code required to install them in the table
; used by the program above.

; c.  Choose any additional differentiation rule that you like, such
; as the one for exponents (exercise 2.56), and install it in this
; data-directed system.

(define (install-deriv-package)
  
  (define (=number? exp num)
    (and (number? exp) (= exp num))) 

  ; sum
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (addend opds) (car opds))
  
  (define (augend opds) (cadr opds))
  
  (define (deriv-sum opds var)
    (make-sum (deriv (addend opds) var)
              (deriv (augend opds) var)))

  ; product
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (multiplier opds) (car opds))
  
  (define (multiplicand opds) (cadr opds))
  
  (define (deriv-product opds var) 
    (make-sum 
     (make-product (multiplier opds)
                   (deriv (multiplicand opds) var))
     (make-product (deriv (multiplier opds) var)
                   (multiplicand opds))))

  ; exponentiation
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list '** base exp))))
  
  (define (base opds) (car opds))
  
  (define (exponent opds) (cadr opds))
  
  (define (deriv-exponentation opds var)
    (make-product
     (exponent opds)
     (make-product
      (make-exponentiation (base opds)
                           (make-sum (exponent opds) (- 1)))
      (deriv (base opds) var))))

  ;; interface
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentation) 
  'done)

; d.  In this simple algebraic manipulator the type of an expression
; is the algebraic operator that binds it together. Suppose, however,
; we indexed the procedures in the opposite way, so that the dispatch
; line in deriv looked like

; ((get (operator exp) 'deriv) (operands exp) var)

; What corresponding changes to the derivative system are required?

; Answer:
; Interface of the derivative system will have to change. The order of
; arguments to put will be switched.

(define (square n) (* n n))

(define (apply-generic op arg) (arg op))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; Exercise 2.75.  Implement the constructor make-from-mag-ang in message-passing 
; style. This procedure should be analogous to the make-from-real-imag procedure
; given above.

(define (make-from-mag-angle r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANGLE" op))))
  dispatch)

; (apply-generic 'imag-part (make-from-real-imag 2 3))