#lang racket
(provide (all-defined-out))
(require "Laminations.rkt")
(require math/number-theory)

; Int (d) x Int (p) ---> Int (d^p - 1)
(define (periodic-denom d p)
  (sub1 (expt d p)))

; Int (d) x Int (p) ---> Frac (1 / d^p -1)
(define (periodic-spacing d p)
  (/ 1 (periodic-denom d p)))

; Int (d) x Int (p) ---> List of Fracs
(define (gen-period-p-pts d p)
  (define (gen-init-pts denom) (cdr (build-list denom (lambda (x) (/ x denom)))))
  (filter (lambda (pt) (true-period-p? d p pt)) (gen-init-pts (periodic-denom d p))))

; Int (d) x Int (p) ---> List of Fracs (lengths)
(define (gen-period-p-leaf-lengths d p)
  (let ([spacing (periodic-spacing d p)])
    (range spacing (+ (/ 1 2) spacing) spacing)))

; Int (d) x Int (p) ---> List of Lists (of length orbits)
(define (gen-period-p-length-orbits d p)
  (define (iter length-orbits leaf-lengths)
    (cond [(null? leaf-lengths) (reverse length-orbits)]
          [else (let ([length-orbit (gen-periodic-length-orbit d (car leaf-lengths))])
                  (iter (cons length-orbit length-orbits) (remove* length-orbit leaf-lengths)))]))
  (iter '() (gen-period-p-leaf-lengths d p)))

; Int (d) x Int (p) ---> List of Fracs (lengths)
(define (gen-minor-lengths d p)
  (map car (gen-period-p-length-orbits d p)))

; Int (d) x Int (p) x Frac (length) ---> List of 2-Lists (minors)
(define (gen-Q1-same-length-periodic-minors d p length)
  (define period-p-pts (gen-period-p-pts d p))
  (define (one-endpoint-in-Q1? leaf)
    (or (and (<= 0 (leaf-a leaf) (/ 1 4)))
        (and (<= 0 (leaf-b leaf) (/ 1 4)))))
  (filter (lambda (leaf) (and (one-endpoint-in-Q1? leaf)
                              (member (leaf-a leaf) period-p-pts)
                              (member (leaf-b leaf) period-p-pts)
                              (true-period-p? d p leaf)
                              (laminational? d leaf)))
          (map (lambda (a) (make-leaf a (fractional-part (+ a length))))
               (range 0 1 (periodic-spacing d p)))))

; Int (d) x Int (p) ---> List of 2-Lists (minors with at least one endpoint in 1st Quadrant)
(define (gen-Q1-periodic-minors d p)
  (define (iter minors minor-lengths)
    (cond [(null? minor-lengths) minors]
          [(iter (append minors (gen-Q1-same-length-periodic-minors d p (car minor-lengths))) (cdr minor-lengths))]))
  (iter '() (gen-minor-lengths d p)))

; Frac (point) x 2-list (leaf) ---> Boolean
(define (pt-on-right? pt leaf)
  (let ([a (leaf-a leaf)] [b (leaf-b leaf)])
    (if (< a b)
        (< a pt b)
        (or (< pt b) (< a pt)))))

; Int (d) x Frac x List of 2-Lists (leaves) ---> List of 2-Lists (Critical Chords of length 1/d)
(define (gen-deg2-crit-chords-from-pt d pt collection)
  (filter (lambda (crit) (not (leaf-cross-collection? crit collection))) (list (make-leaf pt (fractional-part (+ (/ 1 d) pt)))
                                                                               (make-leaf pt (fractional-part (+ (- 1 (/ 1 d)) pt))))))

; Int (d) x 2-List (leaf) ---> List of 2-Lists (Critical Chords of length 1/d)
(define (gen-deg2-leaf-a-crits d minor)
  (define orbit (gen-periodic-orbit d minor))
  (define (iter left-of-orbit crits)
    (if (null? left-of-orbit)
        crits
        (iter (cdr left-of-orbit)
              (append crits (filter (lambda (crit) (pt-on-right? (leaf-b crit) (car left-of-orbit)))
                                    (gen-deg2-crit-chords-from-pt d (leaf-a (car left-of-orbit)) orbit))))))
  (iter orbit '()))

; Int (d) x 2-List (minor) ---> Boolean
(define (2x2-minor? d minor)
  (define orbit (gen-periodic-orbit d minor))
  (define (iter num-of-crits-on-right remaining-orbit)
    (cond [(null? remaining-orbit)
           (= 2 num-of-crits-on-right)]
          [else
           (define crits (filter (lambda (crit) (pt-on-right? (leaf-b crit) (car remaining-orbit)))
                                 (gen-deg2-crit-chords-from-pt d (leaf-a (car remaining-orbit)) orbit)))
           (if (null? crits)
               (iter num-of-crits-on-right (cdr remaining-orbit))
               (iter (add1 num-of-crits-on-right) (cdr remaining-orbit)))])) 
  (iter 0 (cdr orbit)))

; Int (d) x 2-list (leaf) ---> Boolean
(define (2x2-base-minor? d leaf)
  (and (endpoints-different-orbit? d leaf) (2x2-minor? d leaf)))

; Int (d) x 2-list (minor) --> Boolean
(define (gen-Q1-base-leaves d p)
  (filter (lambda (x) (2x2-base-minor? d x))
          (filter (lambda (minor) (> (leaf-length minor) (/ 2 (periodic-denom d p)))) (gen-Q1-periodic-minors d p))))