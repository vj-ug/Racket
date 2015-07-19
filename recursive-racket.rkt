#lang racket
; 1  recursive reverse in racket

(define (my-reverse s)
  (define (reverse-iter s result)
    (if (null? s)
        result
        (reverse-iter (cdr s) (cons (car s) result))))
  (reverse-iter s '()))


; 2  recursive map in racket
(define (my-map f s)
  (define (map-iter s result)
    (if (null? s)
        (reverse result)
        (map-iter (cdr s) (cons (f (car s)) result))))
  (map-iter s '()))
 
(define (reverse s)
  (define (reverse-iter s result)
    (if (null? s)
        result
        (reverse-iter (cdr s) (cons (car s) result))))
  (reverse-iter s '()))

; 3
(define (function-3 func)
  (func 3))

; 4
(define (zipper l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zipper (cdr l1) (cdr l2)))))

; 5

(define (segregate lst)
  (if (null? lst)
      '(null null) 
      (list (my-even lst) (my-odd lst)))) 

; Returns list of even integers 
(define (my-even lst)
  (if (null? lst)
      null 
      (if (even? (car lst))
          (cons (car lst) (my-even (cdr lst))) 
          (my-even (cdr lst)))))

; Returns list of odd integers 
(define (my-odd lst)
  (if (null? lst) 
      null 
      (if (odd? (car lst)) 
          (cons (car lst) (my-odd (cdr lst)))
          (my-odd (cdr lst)))))

; 6

(define (is-member? num lst)
  (if (null? lst)
      #f
      (if (string? num)
          (is-member-string? num lst)
          (is-member-number? num lst))))

(define (is-member-string? num lst)
  (if (null? lst)
      #f
      (if (integer? (car lst))
          (is-member-string? num (rest lst))
          (if (string=? (car lst) num)
              #t
              (is-member-string? num (rest lst))))))

(define (is-member-number? num lst)
  (if (null? lst)
      #f
      (if (string? (car lst))
          (is-member-number? num (rest lst))
          (if (= (car lst) num)
              #t
              (is-member-number? num (rest lst))))))

; 7
(define (my-sorted-int? lst)
  (define size (length lst))
  (if (< size 2)
      #t
      (if (null? lst)
          #t
          (if (> (car lst) (car (rest lst)))
              #f
              (my-sorted-int? (rest lst) )))))

(define (my-sorted-string? lst)
  (define size (length lst))
  (if (< size 2)
      #t
      (if (null? lst)
          #t
          (if (string>? (car lst) (car (rest lst)))
              #f
              (my-sorted-string? (rest lst) )))))

(define (my-sorted? lst)
  (if (string? (car lst))
      (my-sorted-string? lst)
      (my-sorted-int? lst)))

; 8

(define (my-flatten lst)
  (if (null? lst) ; check list null
      null ; 
      (if (list? (car lst)) ; 
          (append (my-flatten (car lst)) (my-flatten (cdr lst))) 
          (append (list (car lst)) (my-flatten (cdr lst))))))

; 9

(define (threshold pred lst)
  (cond [(empty? lst) empty]
        [(pred (first lst))
         (cons (first lst) (threshold pred (rest lst)))]
        [else (threshold pred (rest lst))]))

; (threshold (lambda (x) (> x 5)) '(1 6 7))

; 10

(define my-list-ref
    (lambda (lst place)
      (if (null? lst)
          (display '"index out of bound")
          (if (= place 0)
          (car lst)
          (my-list-ref (cdr lst) (- place 1))))))

; 11 Bonus

 (define (deep-reverse ls)
  (define (deep-reverse-2 ls acc)
    (if (null? ls)
        acc
        (if (list? (car ls))
            (deep-reverse-2 (cdr ls) (cons (deep-reverse (car ls)) acc))
            (deep-reverse-2 (cdr ls) (cons (car ls) acc)))))
  (deep-reverse-2 ls '()))
  
; TEST
(printf "(my-reverse '(1 2 3)): ~v~n" (my-reverse '(1 2 3)))
(printf "(my-map sqrt '(1 4 9)): ~v~n" (my-map sqrt '(1 4 9)))
(printf "(function-3 sqrt): ~v~n" (function-3 sqrt))
(printf "(zipper '(1 2 3 4) '(a b c d)) : ~v~n"  (zipper '(1 2 3 4) '(a b c d)))
(printf "(segregate '(7 2 3 5 8): ~v~n"   (segregate '(7 2 3 5 8)))
(printf "(is-member? 6 '(4 8 6 2 1)) : ~v~n"   (is-member? 6 '(4 8 6 2 1)))
(printf "(my-sorted? '(2 5 6 9 11 34))  : ~v~n"   (my-sorted? '(2 5 6 9 11 34)) )
(printf "(my-flatten '((1 2) 3)) : ~v~n"   (my-flatten '((1 2) 3)) )
(printf "(threshold (lambda (x) (> x 5)) '(1 6 7)): ~v~n" (threshold (lambda (x) (> x 5)) '(1 6 7)))
(printf "(my-list-ref '(1 2) 0) : ~v~n"  (my-list-ref '(1 2) 0) )
(printf "(deep-reverse '(1 2 3)) : ~v~n"   (deep-reverse '(1 2 3)) )

                                                           
 
