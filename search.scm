(define usd '$)
(define nil '())
(define sorted-list '(2 6 10 14 55 65 78 99 102))
(define unsorted-list '(6 7 4 45 7 76 3 67 7 63 19))
(define binary-search-tree '(10 (8 (4 () (6 () ())) (9 () ()) ) (15 () (18 () ())) ))


; purpose:  check if second param is null
; input:    two param (likely two data structures)
; output:   #t is second param is null, #f otherwise
(define (second_empty first second) (equal? second '() ))


; purpose:  get the tail of the second param
; input:    two param
; output:   the tail of the second param
(define (cdr_second a b) (cdr b))


; purpose:  check if the current element of a sorted list is greater than the needle
; input:    current element and a needle
; output:   #t if needle is less than current element, #f otherwise
(define (done-sorted element current)
  (if (null? current) nil
    (< element (car current))
  )
)


; purpose:  get left subtree of a bst
; input:    a bst
; output:   the left subtree
(define (bst-left bst) (cadr bst))


; purpose:  get right subtree of a bst
; input:    a bst
; output:   the right subtree
(define (bst-right bst) (caddr bst))


; purpose:  decide which subtree to search
; input:    a bst and an element
; output:   subtree to search
(define (bst-next element bst)
  (if (< element (car bst))
    (bst-left bst)
    (bst-right bst)
  )
)


; purpose:  search a data structure for an element
; input:    a data structure, an element to search for, a function to get
;           the current-item of data structure, function to check for end
;           of the data structure, a function to check if element is found,
;           and a function to get the next element of the data structure
; output:   empty list if not found, a singleton list containing needle if found
(define (search ds e current-item done found next)
  (cond
    ((done e ds) nil)
    ((found e (current-item ds))  (list (current-item ds)))
    (else     (search (next e ds) e current-item done found next))
  )
)


; purpose:  search a data structure for an element and return a list that contains
;           $ every time next is called
; input:    a data structure, an element to search for, a function to get
;           the current-item of data structure, function to check for end
;           of the data structure, a function to check if element is found,
;           and a function to get the next element of the data structure
; output:   empty list if not found, a list containg n (times next was called)
;           number of $ characters with the needle at the end
(define (search-ec ds e current-item done found next)
  (cond
    ((done e ds) nil)
    ((found e (current-item ds))   (list (current-item ds)))
    (else     (cons usd (search-ec (next e ds) e current-item done found next)))
  )
)


; call to search an unsorted list
(define (list-search-unsorted list e)
  (search list e car second_empty equal? cdr_second)
)

; call to search a sorted list
(define (list-search-sorted list e)
  (search list e car done-sorted equal? cdr_second)
)

; call to search an unsorted list w/ extra credit
(define (list-search-ec list e)
  (search-ec list e car second_empty equal? cdr_second)
)

; call to search a bst
(define (bst-search bst e)
  (search bst e car second_empty equal? bst-next)
)
