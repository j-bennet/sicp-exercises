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
; ---------------- Huffman trees ----------------

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; The following procedure implements the decoding algorithm.
; It takes as arguments a list of zeros and ones, together with a
; Huffman tree.

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Exercise 2.67.  Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; Use the decode procedure to decode the message, and give the
; result.
; (decode sample-message sample-tree)

; Answer: (A D A B B C A)

; Exercise 2.68.  The encode procedure takes as arguments a
; message and a tree and produces the list of bits that
; gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Encode-symbol is a procedure, which you must write, that
; returns the list of bits that encodes a given symbol according
; to a given tree. You should design encode-symbol so that it
; signals an error if the symbol is not in the tree at all.
; Test your procedure by encoding the result you obtained in
; exercise 2.67 with the sample tree and seeing whether it
; is the same as the original sample message.

(define (in-list? x xs)
  (cond [(null? xs) #f]
        [(equal? x (car xs)) #t]
        [else (in-list? x (cdr xs))]))

(define (encode-symbol symbol tree)
  (if (in-list? symbol (symbols tree))
      (if (leaf? tree) 
          '()
          (let ((l (left-branch tree))
                (r (right-branch tree)))
            (if (in-list? symbol (symbols l))
                (cons 0 (encode-symbol symbol l))
                (cons 1 (encode-symbol symbol r)))))
      (error "symbol is not in tree" symbol tree)))

; Answer:
; (encode '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
; (0 1 1 0 0 1 0 1 0 1 1 1 0) - same as sample message

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; The following procedure takes a list of symbol-frequency pairs
; such as ((A 4) (B 2) (C 1) (D 1)) and constructs an initial
; ordered set of leaves, ready to be merged according to the
; Huffman algorithm:

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.69.  The following procedure takes as its argument
; a list of symbol-frequency pairs (where no symbol appears
; in more than one pair) and generates a Huffman encoding
; tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms
; the list of pairs into an ordered set of leaves. Successive-merge
; is the procedure you must write, using make-code-tree
; to successively merge the smallest-weight elements of
; the set until there is only one element left, which is
; the desired Huffman tree. (This procedure is slightly tricky,
; but not really complicated. If you find yourself designing a
; complex procedure, then you are almost certainly doing
; something wrong. You can take significant advantage of
; the fact that we are using an ordered set representation.)

(define (successive-merge ls)
  (cond [(null? ls) '()]
        [(= 1 (length ls)) (car ls)]
        [else (successive-merge 
               (cons
                (make-code-tree (car ls) (cadr ls))   ; take first two
                (cddr ls)))]))                        ; drop first two and take the rest

; Exercise 2.70.  The following eight-symbol alphabet with
; associated relative frequencies was designed to efficiently
; encode the lyrics of 1950s rock songs. (Note that the
; ``symbols'' of an ``alphabet'' need not be individual letters.)

; A	2	NA	16
; BOOM	1	SHA	3
; GET	2	YIP	9
; JOB	2	WAH	1
; Use generate-huffman-tree (exercise 2.69) to generate a
; corresponding Huffman tree, and use encode (exercise 2.68)
; to encode the following message:

; Get a job

; Sha na na na na na na na na

; Get a job

; Sha na na na na na na na na

; Wah yip yip yip yip yip yip yip yip yip

; Sha boom

(define (song-tree)
  (generate-huffman-tree '(('A 2) ('BOOM 1) ('GET 2) ('JOB 2) ('NA 16) ('SHA 3) ('YIP 9) ('WAH 1))))

(define (song-tree-encoded)
  (encode '('GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'SHA 'BOOM) (song-tree)))

; How many bits are required for the encoding?

; Answer:
; (length (song-tree-encoded))
; length is 87

; What is the smallest number of bits that would be needed
; to encode this song if we used a fixed-length code for
; the eight-symbol alphabet?

; Answer:
; (* (+ 2 1 2 2 16 3 9 1) 3)
; 108
