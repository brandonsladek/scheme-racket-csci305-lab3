#lang racket/base

; Warmup function
(define (f lst)
  ; (a) Check if lst is null (empty) 
  (if (null? lst)
      ; (b) If lst is null, return empty list 
      '()
      ; (c) Otherwise, recursively increment each value by one 
      (cons (+ 1 (car lst)) (f(cdr lst)))))

; Determine if e is a member of lst 
(define (member? e lst)
  (cond
    ; If we reach the end of the list, e is not in the list
    [(null? lst) #f]
    ; If the first element of the list is e
    [(eqv? e (car lst)) #t]
    ; If it isn't e, check if e is in the rest of the list
    [else (member? e (cdr lst))]))

; Determine if lst is a well formed set 
(define (set? lst)
  (cond
    ; If we reach the end of the list, there are no repeat elements
    [(null? lst) #t]
    ; If the first element is a repeat element
    [(member? (car lst) (cdr lst)) #f]
    ; Otherwise, check rest of list
    [else (set? (cdr lst))]))

; Return the union of two sets
(define (union lst1 lst2)
  ; Remove repeat elements in both lists and concatenated list
   (remover (append (remover lst1) (remover lst2))))
  
; Remove repeat elements in a list
(define (remover lst)
  (cond
    [(null? lst) '()]
    ; If the first element is a repeat element
    [(member? (car lst) (cdr lst))
     ; Ignore it and move to the next element
     (remover (cdr lst))]
    ; Otherwise, concatenate it to the desired list
    [else (cons (car lst) (remover (cdr lst)))]))

; Return the intersection of two sets
(define (inter lst1 lst2)
  (cond
    [(null? lst1) '()]
    ; If the first element of lst1 is in lst2
    [(member? (car lst1) lst2)
     ; Concatenate that element with the intersection set
     (cons (car lst1) (inter (cdr lst1) lst2))]
    ; Otherwise, check out the rest of the list
    [else (inter (cdr lst1) lst2) ]))

; End file
  