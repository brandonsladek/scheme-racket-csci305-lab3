#lang racket/base

; Warmup function
(define (f lst)
  ; (a) Check if lst is null (empty) ;
  (if (null? lst)
      ; (b) If lst is null, return empty list ;
      '()
      ; (c) Otherwise, recursively increment each value by one ;
      (cons (+ 1 (car lst)) (f(cdr lst)))))

; Determines if e is a member of lst ;
(define (member? e lst)
  (cond
    [(null? lst) #f]
    [(eqv? e (car lst)) #t]
    [else (member? e (cdr lst))]))

; Determines if lst is a well formed set ;
(define (set? lst)
  (cond
    [(null? lst) #t]
    [(member? (car lst) (cdr lst)) #f]
    [else (set? (cdr lst))]))

; Return the union of two sets ;
(define (union lst1 lst2)
  (cond
    [(and (null? lst1) (null? lst2)) '()]
    [(null? lst2) '()]
    [else (cons lst1 car lst2)]))
  
  
  
  
  
  