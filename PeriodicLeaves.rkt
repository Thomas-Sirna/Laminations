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

; 1) Generate all periodic points under sigma_d
; 2) Generate all leaves with enpoints of this period - (combinations ...)
; 3) Map the orbits onto these leaves
; 4) Filter out all leaves whose orbits cross themselves  -|__> (filter-map ...)
; 5) Filter out all those orbits with a crossing leaf     -|

(define s3-period2-points (gen-period-p-pts 3 2))
(define s3-period2-leaves (combinations s3-period2-points 2))
(define s3-period2-laminational-leaves (filter (lambda (leaf) (laminational? 3 leaf)) s3-period2-leaves))
(define s3-period2-compatible-leaf-orbits (map (lambda (leaf) (gen-periodic-orbit 3 leaf)) s3-period2-laminational-leaves))

(define s3-period3-points (gen-period-p-pts 3 3))
(define s3-period3-leaves (combinations s3-period3-points 3))
(define s3-period3-laminational-leaves (filter (lambda (leaf) (laminational? 3 leaf)) s3-period3-leaves))
(define s3-period3-compatible-leaf-orbits (map (lambda (leaf) (gen-periodic-orbit 3 leaf)) s3-period3-laminational-leaves))


(define (point-to-string point)
  (define n (length point))
  (define (iter string left)
    (cond [(null? left) string]
          [(equal? '_ (car left)) (iter (string-append string "_") (cdr left))]
          [(equal? 0 (car left)) (iter (string-append string "0") (cdr left))]
          [(equal? 1 (car left)) (iter (string-append string "1") (cdr left))]
          [(equal? 2 (car left)) (iter (string-append string "2") (cdr left))]
          [(equal? 3 (car left)) (iter (string-append string "3") (cdr left))]
          [(equal? 4 (car left)) (iter (string-append string "4") (cdr left))]
          [(equal? 5 (car left)) (iter (string-append string "5") (cdr left))]
          [(equal? 6 (car left)) (iter (string-append string "6") (cdr left))]
          [(equal? 7 (car left)) (iter (string-append string "7") (cdr left))]
          [(equal? 8 (car left)) (iter (string-append string "8") (cdr left))]
          [(equal? 9 (car left)) (iter (string-append string "9") (cdr left))]))
  (iter "" point))




(define (make-json d p leaf1 leaf2 crit1 crit2)
  (define filename (string-append (point-to-string (leaf-a leaf1)) (point-to-string (leaf-b leaf1))
                                  (point-to-string (leaf-a leaf2)) (point-to-string (leaf-b leaf2)) ".json"))
  (define a1 (point-to-string (leaf-a leaf1)))
  (define b1 (point-to-string (leaf-b leaf1)))
  (define a2 (point-to-string (leaf-a leaf2)))
  (define b2 (point-to-string (leaf-b leaf2)))
  (define x (point-to-string (leaf-a crit1)))
  (define y (point-to-string (leaf-b crit1)))
  (define w (point-to-string (leaf-a crit2)))
  (define z (point-to-string (leaf-b crit2)))
  (with-output-to-file filename (lambda () (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbits of Minor Chords ~a-~a and ~a-~a.\",
  \"branches\": [
    {
      \"chord\": [\"~a\", \"~a\"],
      \"endpoints\": [\"~a\"]
    },
{
      \"chord\": [\"~a\", \"~a\"],
      \"endpoints\": [\"~a\"]
    },
  ],

\"leaves\": [
	{\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"black\", \"fillColor\": \"orange\"}},

]
}"
p d a1 b1 a2 b2 x x x x x x x a1 b1 a1 a2 b2 a2))))