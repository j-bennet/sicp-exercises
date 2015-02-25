#lang scheme

; Reading: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3

; Selected exercises:

; 3.13
; 3.18

; The above two exercises are tricky and will require some thought. They'll
; also be good practice for exploring mutability in scheme.

; The other sections of the chapter cover queues, tables, and a digital circuit 
; simulator. They are all great practice but not required for discussion or
; for learning the basic concepts from the chapter, so it'll be up to all of
; you if you'd like to spend any time on them. We can definitely still discuss
; them at the meeting.

; Exercise 3.13.  Consider the following make-cycle procedure, which uses the last-pair procedure defined in exercise 3.12:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

; Draw a box-and-pointer diagram that shows the structure z created by

; (define z (make-cycle (list 'a 'b 'c)))

; What happens if we try to compute (last-pair z)?

; Answer: it would never end

; Exercise 3.18.  Write a procedure that examines a list and determines whether it contains
; a cycle, that is, whether a program that tried to find the end of the list by taking
; successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.
 (define (has-cycle? l) 
   (define (detect pair countedList) 
     (cond ((not (pair? pair)) #f) 
           ((memq pair countedList) #t) 
           (else (detect (cdr pair) (cons pair countedList))))) 
   (detect l '())) 
