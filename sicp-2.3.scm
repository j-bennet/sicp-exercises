#lang scheme
; Reading: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3
;
; Selected exercises:
;
; 2.53
; 2.54
; 2.56
; 2.63
; 2.67
;
; Optional (but fun!): 2.68-2.70 inclusive (sha na na na na na na...)

; If the symbol is not contained in the list (i.e., is not eq? to any item in the
; list), then memq returns false. Otherwise, it returns the sublist of the
; list beginning with the first occurrence of the symbol.

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.53.  What would the interpreter print in response to 
; evaluating each of the following expressions?

;(list 'a 'b 'c)
; (a b c)

;(list (list 'george))
; ((georde))

;(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

;(cadr '((x1 x2) (y1 y2)))
; (y 1 y 2)

;(pair? (car '(a short list)))
; #f

;(memq 'red '((red shoes) (blue socks)))
; #f

;(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54.  Two lists are said to be equal? if they contain equal
; elements arranged in the same order. For example,
;   (equal? '(this is a list) '(this is a list))
; is true, but
;   (equal? '(this is a list) '(this (is a) list))
; is false. To be more precise, we can define equal? recursively
; in terms of the basic eq? equality of symbols by saying that a and
; b are equal? if they are both symbols and the symbols are eq?, or
; if they are both lists such that (car a) is equal? to (car b) and
; (cdr a) is equal? to (cdr b). Using this idea, implement equal?
; as a procedure.

(define (equals? a b)
  (cond 
    [(and (null? a) (null? b)) #t]
    [(or (null? a) (null? b)) #f]
    [(and (list? a) (list? b))
     (and 
      (equals? (car a) (car b)) 
      (equals? (cdr a) (cdr b)))]
    [(eq? a b) #t]
    [else #f]))

; The variables are symbols. They are identified by the primitive
; predicate symbol?:
(define (variable? x) (symbol? x))

; Two variables are the same if the symbols representing them are eq?:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;Checks whether an expression is equal to a given number.
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Sums and products are constructed as lists:
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; It's a list, but we also handle 0 and 0.
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; The addend is the second item of the sum list:
(define (addend s) (cadr s))

; The augend is the third item of the sum list:
(define (augend s) (caddr s))

; A product is a list whose first element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

; The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))
; Exercise 2.56.  Show how to extend the basic differentiator to
; handle more kinds of expressions. For instance, implement the
; differentiation rule
; d (u^n) / dx = (n * u^(n - 1)) * (du / dx)
; by adding a new clause to the deriv program and defining
; appropriate procedures exponentiation?, base, exponent,
; and make-exponentiation. (You may use the symbol ** to
; denote exponentiation.) Build in the rules that anything
; raised to the power 0 is 1 and anything raised to the power
; 1 is the thing itself.

; Exponentiation is a list whose first element is the symbol **:
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

; The base is the second item of the exponentiation list:
(define (base x) (cadr x))

; The exponent is the third item of the exponentiation list:
(define (exponent x) (caddr x))

; Creates a list of '**, base, exponent
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((=number? b 0) 0)
        (else (list '** b e))))

; Derivative of expression with respect to a variable.
; For example, if the arguments to the procedure are
; ax2 + bx + c and x, the procedure should return 2ax + b.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ([b (base exp)]
               [e (exponent exp)])
           (make-product (make-product b (make-exponentiation b (- e 1)))
                         (deriv b var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; Defining trees.
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (make-test-leaf entry)
  (list entry null null))

; Exercise 2.63.  Each of the following two procedures converts a binary
; tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; a. Do the two procedures produce the same result for every tree?
; If not, how do the results differ? What lists do the two
; procedures produce for the trees in figure 2.16?

(define (make-test-tree-1)
  (make-tree
   7
   (make-tree 3 (make-test-leaf 1) (make-test-leaf 5))
   (make-tree 9 null (make-test-leaf 11))))

(define (make-test-tree-2)
  (make-tree
   3
   (make-test-leaf 1)
   (make-tree
    7
    (make-test-leaf 5)
    (make-tree 9 null (make-test-leaf 11)))))

(define (make-test-tree-3)
  (make-tree
   5
   (make-tree 3 (make-test-leaf 1) null)
   (make-tree 9 (make-test-leaf 7) (make-test-leaf 11))))

; Answer: they return the same result.
; For the 3 test trees above, it's '(1 3 5 7 9 11)

; b. Do the two procedures have the same order of growth in the
; number of steps required to convert a balanced tree with n
; elements to a list? If not, which one grows more slowly?

; Answer: both of them recursively go through all nodes of tree,
; which makes it O(n).The difference is that
; v1 uses (append left entry right), whereas 
; v2 uses (copy-to-list left entry right).
