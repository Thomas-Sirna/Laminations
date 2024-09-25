#lang racket
(provide (all-defined-out))
(require math/number-theory)

; Frac --> Frac (< 1)
(define (fractional-part x) (- x (truncate x)))

; Frac (point) x Frac (point) ---> 2-List (leaf)
(define make-leaf list)

; 2-List (leaf) ---> Frac (first endpoint)
(define leaf-a car)

; 2-List (leaf) ---> Frac (second endpoint)
(define leaf-b cadr)

; 2-List (leaf) ---> Frac (length between 0 and 1/2)
(define (leaf-length leaf)
  (let ([raw_length (abs (- (leaf-b leaf) (leaf-a leaf)))])
    (if (<= raw_length (/ 1 2))
        raw_length
        (- 1 raw_length))))

; 2-List (leaf) x 2-list (leaf) ---> Boolean
(define (>leaf-length? l1 l2)
  (> (leaf-length l1) (leaf-length l2)))

; 2-List (leaf) x 2-list (leaf) ---> Boolean
(define (leaves-cross? l1 l2)
  (define (order-leaf leaf) (sort leaf <))
  (define (order-leaves-by-first-endpoint leaf-list)
    (sort leaf-list (lambda (l1 l2) (<= (leaf-a l1) (leaf-a l2)))))
  (define (ordered-leaves-cross? l1 l2)
    (let ([a (leaf-a l1)] [b (leaf-b l1)] [x (leaf-a l2)] [y (leaf-b l2)])
      (and (< a x) (< x b) (< b y))))
  (define ordered-leaves-list (order-leaves-by-first-endpoint (list (order-leaf l1) (order-leaf l2))))
  (ordered-leaves-cross? (first ordered-leaves-list) (second ordered-leaves-list)))

; Int (d) x Frac or 2-List (leaf) --> Frac or 2-list (leaf)
(define (sigma-d d pt-or-leaf)
  (cond [(rational? pt-or-leaf)
         (fractional-part (* d pt-or-leaf))]
        [else
         (map (lambda (pt) (fractional-part (* d pt))) pt-or-leaf)]))

; Int (d) x Int (n) x Frac or 2-list (leaf) --> Frac or 2-list (leaf)
(define (sigma-d^n d n pt-or-leaf)
  (cond [(= 0 n) pt-or-leaf]
        [else (sigma-d^n d (sub1 n) (sigma-d d pt-or-leaf))]))

; Int (d) x Frac (leaf length) ---> Frac (leaf length)
(define (leaf-length-sigma-d d length)
  (leaf-length (sigma-d d (make-leaf 0 length))))

; Int (d) x Frac or 2-List (leaf) ---> p-List of 2-Lists (leaves)
(define (gen-periodic-orbit d pt-or-leaf)
  (define (iter orbit current-pt-or-leaf)
    (cond [(equal? current-pt-or-leaf pt-or-leaf) (reverse orbit)]
          [else (iter (cons current-pt-or-leaf orbit) (sigma-d d current-pt-or-leaf))]))
  (iter (list pt-or-leaf) (sigma-d d pt-or-leaf)))

; Int (d) x Frac (leaf-length) ---> List of Fractions (leaf lengths)
(define (gen-periodic-length-orbit d length)
  (define (iter orbit current-length)
    (cond [(equal? current-length length) (reverse orbit)]
          [else (iter (cons current-length orbit) (leaf-length-sigma-d d current-length))]))
  (iter (list length) (leaf-length-sigma-d d length)))

; Int (d) x Frac or 2-List (leaf) x Frac or 2-list (leaf) ---> Boolean
(define (different-orbit? d pt-or-leaf1 pt-or-leaf2)
  (define orbit (gen-periodic-orbit d pt-or-leaf1))
  (not (member pt-or-leaf2 orbit)))

; Int (d) x 2-list (leaf) ---> Boolean
(define (endpoints-different-orbit? d leaf)
  (different-orbit? d (leaf-a leaf) (leaf-b leaf)))

; 2-List (leaf) x List of 2-Lists (leaves) ---> Boolean
(define (leaf-cross-collection? leaf collection)
  (cond [(null? collection) #f]
        [(leaves-cross? leaf (car collection)) #t]
        [else (leaf-cross-collection? leaf (cdr collection))]))

; Int (d) x 2-List (leaf) ---> Boolean
(define (laminational? d leaf)
  (define orbit (gen-periodic-orbit d leaf))
  (define (iter remaining-orbit)
    (cond [(null? remaining-orbit) #t]
          [(leaf-cross-collection? (car remaining-orbit) orbit) #f]
          [else (iter (cdr remaining-orbit))]))
  (iter orbit))

; Int (d) x 2-List (leaf) x 2-list (leaf) ---> Boolean
(define (orbits-cross? d leaf1 leaf2)
  (define (iter orbit1 orbit2)
    (cond [(null? orbit1) #f]
          [(leaf-cross-collection? (car orbit1) orbit2) #t]
          [else (iter (cdr orbit1) orbit2)]))
  (iter (sort (gen-periodic-orbit d leaf1) >leaf-length?)
        (sort (gen-periodic-orbit d leaf2) >leaf-length?)))

; Int (d) x Int (p) x Frac or 2-List (leaf) ---> Boolean
(define (true-period-p? d p pt-or-leaf)
  (and (equal? pt-or-leaf (sigma-d^n d p pt-or-leaf))
       (not (ormap (lambda (divisor) (equal? pt-or-leaf (sigma-d^n d divisor pt-or-leaf)))
                   (remove p (divisors p))))))

; Int (d) x Frac or 2-List (leaf) ---> Int (period)
(define (period d pt-or-leaf)
  (let loop ([p 1])
    (if (true-period-p? d p pt-or-leaf)
        p
        (loop (add1 p)))))

; Int (d) x Frac (point) ---> p-list
(define (d-nary-periodic-point d point)
  (define p (period d point))
  (define denom (sub1 (expt d p)))
  (define (euclid point position)
    (define (iter left sub-value numeral)
      (if (< (- left sub-value) 0)
          (list numeral left)
          (iter (- left sub-value) sub-value (add1 numeral))))
    (iter point (/ 1 (expt d position)) 0))
  (define (iter n left-of-point d-nary)
    (cond [(> n p) (reverse d-nary)]
          [else (iter (add1 n) (cadr (euclid left-of-point n)) (cons (car (euclid left-of-point n)) d-nary))]))
  (iter 1 point (cons '_ '())))

; Int (d) x 2-List (leaf) ---> 2-List of p-Lists (leaves in d-nary)
(define (d-nary-periodic-leaf d leaf)
  (map (lambda (x) (d-nary-periodic-point d x)) leaf))

; Int (d) x Frac (point) ---> p-list
(define (d-nary-preperiodic-point d point)
  (define p (period d (sigma-d d point)))
  (define denom (sub1 (expt d p)))
  (define (euclid point position)
    (define (iter left sub-value numeral)
      (if (< (- left sub-value) 0)
          (list numeral left)
          (iter (- left sub-value) sub-value (add1 numeral))))
    (iter point (/ 1 (expt d position)) 0))
  (define (iter n left-of-point d-nary)
    (cond [(> (sub1 n) p) (reverse d-nary)]
          [else (iter (add1 n) (cadr (euclid left-of-point n)) (cons (car (euclid left-of-point n)) d-nary))]))
  (define period-digit (car (euclid point 1)))
  (iter 2 (cadr (euclid point 1)) (list '_ period-digit)))

  