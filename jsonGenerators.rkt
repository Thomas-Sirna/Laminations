#lang racket
(provide (all-defined-out))
(require "Laminations.rkt")
(require "BaseLeaves.rkt")
(require math/number-theory)



; Int (d) x Int (p) ---> List of Lists (Orbits of Fracs)
(define (gen-period-p-point-orbits d p)
  (define (iter orbits points)
    (cond [(null? points) (reverse orbits)]
          [else (let ([orbit (gen-periodic-orbit d (car points))])
                  (iter (cons orbit orbits) (remove* orbit points)))]))
  (iter '() (gen-period-p-pts d p)))


; List (point in d-nary) ---> String (point in d-nary)
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




;                                            
;                                            
;   ;;;;;            ;            ;          
;   ;    ;                        ;          
;   ;    ;  ;;;    ;;;   ; ;;   ;;;;;   ;;;  
;   ;   ;; ;; ;;     ;   ;;  ;    ;    ;   ; 
;   ;;;;;  ;   ;     ;   ;   ;    ;    ;     
;   ;      ;   ;     ;   ;   ;    ;     ;;;  
;   ;      ;   ;     ;   ;   ;    ;        ; 
;   ;      ;; ;;     ;   ;   ;    ;    ;   ; 
;   ;       ;;;    ;;;;; ;   ;    ;;;   ;;;  
;                                            
;                                            
;                                            

; Manually change fixed points depending on d
; and change amount of points based on period
; Data is assumed to be in fractions

; Makes a json for a sigma 3 period 3 point orbit, colored green, with preimages of 0 in black. 
(define (make-point-orbit-json d p orbit)
  (define d-nary-orbit (map (lambda (point) (d-nary-periodic-point d point)) orbit))
  (define point1 (point-to-string (first d-nary-orbit)))
  (define point2 (point-to-string (second d-nary-orbit)))
  (define point3 (point-to-string (third d-nary-orbit)))
  (define filename (string-append point1 point2 point3 ".json"))

  (with-output-to-file filename (lambda () (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit ~a-~a-~a.\",
  \"branches\": [
    {
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
{
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
  ],

\"leaves\": [
	{\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
p d point1 point2 point3 point1 point2 point3  ))))

; Given a list of period 3 sigma 3 point orbits, makes jsons for each 
(define (make-period-3-orbit-jsons d p orbits)
  (define (iter orbits)
    (cond [(null? orbits)
           (pretty-print "Done!")]
          [else 
                (make-point-orbit-json d p (car orbits))
                (iter (cdr orbits))]))
  (iter orbits))

; Manually change fixed points depending on d
; and change amount of points based on period
; Data is assumed to be in fractions
; Makes a json for a sigma 3 period 4 point orbit, colored green, with preimages of 0 in black. 
(define (make-point-orbit-json-per4 d p orbit)
  (define d-nary-orbit (map (lambda (point) (d-nary-periodic-point d point)) orbit))
  (define point1 (point-to-string (first d-nary-orbit)))
  (define point2 (point-to-string (second d-nary-orbit)))
  (define point3 (point-to-string (third d-nary-orbit)))
  (define point4 (point-to-string (fourth d-nary-orbit)))
  (define filename (string-append point1 point2 point3 point4 ".json"))

  (with-output-to-file filename (lambda () (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit ~a-~a-~a-~a.\",
  \"branches\": [
    {
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
{
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
  ],

\"leaves\": [
	{\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
p d point1 point2 point3 point4 point1 point2 point3 point4 ))))

; Given a list of period 3 sigma 4 point orbits, makes jsons for each 
(define (make-period-4-orbit-jsons d p orbits)
  (define (iter orbits)
    (cond [(null? orbits)
           (pretty-print "Done!")]
          [else 
                (make-point-orbit-json-per4 d p (car orbits))
                (iter (cdr orbits))]))
  (iter orbits))


; Given a list of sigma 3 period 3 point orbits, makes the appropriate latex commands
; for corresponding figures, three in a row, named by their point orbit 
(define (make-latex-point-orbits d orbits)
  (define d-naryo1 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (first orbits)))
  (define d-naryo2 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (second orbits)))
  (define d-naryo3 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (third orbits)))
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a.png}
\\end{subfigure}
\\end{figure}"
          (first d-naryo1) (second d-naryo1) (third d-naryo1)
          (first d-naryo2) (second d-naryo2) (third d-naryo2)
          (first d-naryo3) (second d-naryo3) (third d-naryo3))
(close-output-port out))

; Given a list of sigma 3 period 4 point orbits, prints the appropriate latex commands
; for corresponding figures, three in a row, named by their point orbit, to "temp.txt"
(define (make-latex-per4-point-orbits d orbits)
  (define d-naryo1 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (first orbits)))
  (define d-naryo2 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (second orbits)))
  (define d-naryo3 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (third orbits)))
  (define d-naryo4 (map (lambda (point) (point-to-string (d-nary-periodic-point d point))) (fourth orbits)))
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{PeriodicPoints/~a~a~a~a.png}
\\end{subfigure}
\\end{figure}"
          (first d-naryo1) (second d-naryo1) (third d-naryo1) (fourth d-naryo1)
          (first d-naryo2) (second d-naryo2) (third d-naryo2) (fourth d-naryo2)
          (first d-naryo3) (second d-naryo3) (third d-naryo3) (fourth d-naryo3))
(close-output-port out))

; Given a list of period 3 point orbits in sigma 3, prints their latex figure commands to "temp.txt"
(define (make-period-3-orbit-latex d p orbits)
  (define (iter orbits)
    (pretty-print orbits)
    (cond [(null? orbits)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbits))
           (make-latex-point-orbits d (list (first orbits) (first orbits) (first orbits)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbits))
           (make-latex-point-orbits d (list (first orbits) (second orbits) (second orbits)))
           (pretty-print "c3: Done!")]
          [else 
                (make-latex-point-orbits d orbits)
                (iter (cdddr orbits))]))
  (iter orbits))

(define (make-period-4-orbit-latex d p orbits)
  (define (iter orbits)
    (pretty-print orbits)
    (cond [(null? orbits)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbits))
           (make-latex-per4-point-orbits d (list (first orbits) (first orbits) (first orbits) (first orbits)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbits))
           (make-latex-per4-point-orbits d (list (first orbits) (second orbits) (second orbits) (second orbits)))
           (pretty-print "c3: Done!")]
          [(null? (cdddr orbits))
           (make-latex-per4-point-orbits d (list (first orbits) (second orbits) (third orbits) (third orbits)))
           (pretty-print "c4: Done!")]
          [else 
                (make-latex-per4-point-orbits d orbits)
                (iter (cddddr orbits))]))
  (iter orbits))

(define per-3-orbits (gen-period-p-point-orbits 3 3))
(define o1 (first per-3-orbits))
(define o2 (second per-3-orbits))
(define o3 (third per-3-orbits))
(define per-4-orbs (gen-period-p-point-orbits 3 4))


;                                                                                                                  
;                                                                                                                  
;   ;                                                      ;;   ;                  ;;;            ;     ;          
;   ;                                                      ;;   ;                 ;   ;                 ;          
;   ;       ;;;   ;;;;   ;   ;   ;;;    ;;;                ;;;  ;   ;;;          ;       ;;;;   ;;;   ;;;;;   ;;;  
;   ;      ;;  ;      ;  ;   ;  ;;  ;  ;   ;               ; ;  ;  ;; ;;         ;       ;;  ;    ;     ;    ;   ; 
;   ;      ;   ;;     ;   ; ;   ;   ;; ;                   ; ;; ;  ;   ;         ;       ;        ;     ;    ;     
;   ;      ;;;;;;  ;;;;   ; ;   ;;;;;;  ;;;                ;  ; ;  ;   ;         ;       ;        ;     ;     ;;;  
;   ;      ;      ;   ;   ; ;   ;          ;               ;  ;;;  ;   ;         ;       ;        ;     ;        ; 
;   ;      ;      ;   ;    ;    ;      ;   ;    ;;         ;   ;;  ;; ;;          ;   ;  ;        ;     ;    ;   ; 
;   ;;;;;;  ;;;;   ;;;;    ;     ;;;;   ;;;     ;;         ;   ;;   ;;;            ;;;   ;      ;;;;;   ;;;   ;;;  
;                                               ;                                                                  
;                                              ;;                                                                  
;                                                                                                                  
                               

; Now will append to file "temp.tex" where I can then copy-paste to the Latex file I'm using
(define (make-latex-period-3-leaf-orbits d orbits)
  (define d-naryo1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits)))
  (define d-naryo2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits)))
  (define d-naryo3 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (third orbits)))
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{AllPeriod3LeafOrbits/~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{AllPeriod3LeafOrbits/~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{AllPeriod3LeafOrbits/~a~a.png}
\\end{subfigure}
\\end{figure}"
           (first (first d-naryo1)) (second (first d-naryo1)) 
           (first (first d-naryo2)) (second (first d-naryo2))
           (first (first d-naryo3)) (second (first d-naryo3)))
  (close-output-port out))

; Iterate to make tex files given list of period 3 leaf orbits
(define (make-period-3-leaf-orbit-latex d p orbits)
  (define (iter orbits)
    (pretty-print orbits)
    (cond [(null? orbits)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbits))
           (make-latex-period-3-leaf-orbits d (list (first orbits) (first orbits) (first orbits)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbits))
           (make-latex-period-3-leaf-orbits d (list (first orbits) (second orbits) (second orbits)))
           (pretty-print "c3: Done!")]
          [else 
           (make-latex-period-3-leaf-orbits d orbits)
           (iter (cdddr orbits))]))
  (iter orbits))

; Makes a json of a period 3 leaf orbit, no critical leaves
(define (make-period-3-leaf-orbit-json d p orbit)
  (define d-nary-orbit (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit))
  (define leaf1 (first d-nary-orbit))
  (define leaf2 (second d-nary-orbit))
  (define leaf3 (third d-nary-orbit))
  (define filename (string-append (leaf-a leaf1) (leaf-b leaf1) ".json"))

  (with-output-to-file filename (lambda ()
                                  (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit of Minor ~a-~a.\",
  \"branches\": [
    {
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
{
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
  ],

\"leaves\": [
	{\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
                                          p d (leaf-a leaf1) (leaf-b leaf1) (leaf-a leaf1) (leaf-b leaf1) (leaf-a leaf2) (leaf-b leaf2) (leaf-a leaf3) (leaf-b leaf3)
                                          (leaf-a leaf1) (leaf-a leaf2) (leaf-a leaf3)))))

; Iterate to make jsons given list of period 3 leaf orbits
(define (make-period-3-leaf-orbit-jsons d p orbits)
  (define (iter orbits)
    (cond [(null? orbits)
           (pretty-print "Done!")]
          [else 
           (make-period-3-leaf-orbit-json d p (car orbits))
           (iter (cdr orbits))]))
  (iter orbits))


; Int (d) x Int (p) x Frac (length) ---> List of 2-Lists (minors)
(define (gen-possible-same-length-periodic-minors d p length)
  (define period-p-pts (gen-period-p-pts d p))
  (filter (lambda (leaf) (and (member (leaf-a leaf) period-p-pts)
                              (member (leaf-b leaf) period-p-pts)))
          (map (lambda (a) (make-leaf a (fractional-part (+ a length))))
               (range 0 1 (periodic-spacing d p)))))

(define (gen-period-p-minors d p)
  (define period-p-minor-lengths (gen-minor-lengths d p))
  (define (iter minors minor-lengths)
    (cond [(null? minor-lengths)
           (reverse minors)]
          [else
           (iter (append (gen-possible-same-length-periodic-minors d p (car minor-lengths)) minors)
                 (cdr minor-lengths))]))
  (iter '() period-p-minor-lengths))
(define period-3-minors (reverse (sort (gen-period-p-minors 3 3) >leaf-length?)))
(define period-3-orbits (map (lambda (minor) (gen-periodic-orbit 3 minor)) period-3-minors))
; (make-period-3-leaf-orbit-latex 3 3 (filter (lambda (orbit) (laminational? 3 (car orbit))) period-3-orbits))
; (make-period-3-leaf-orbit-jsons 3 3 period-3-orbits)




;                                                                                                                  
;                                                                      ;                                           
;   ;                                                                  ;           ;;;            ;     ;          
;   ;                                                                  ;          ;   ;                 ;          
;   ;       ;;;   ;;;;   ;   ;   ;;;    ;;;          ;;;;   ; ;;    ;;;;         ;       ;;;;   ;;;   ;;;;;   ;;;  
;   ;      ;;  ;      ;  ;   ;  ;;  ;  ;   ;             ;  ;;  ;  ;; ;;         ;       ;;  ;    ;     ;    ;   ; 
;   ;      ;   ;;     ;   ; ;   ;   ;; ;                 ;  ;   ;  ;   ;         ;       ;        ;     ;    ;     
;   ;      ;;;;;;  ;;;;   ; ;   ;;;;;;  ;;;           ;;;;  ;   ;  ;   ;         ;       ;        ;     ;     ;;;  
;   ;      ;      ;   ;   ; ;   ;          ;         ;   ;  ;   ;  ;   ;         ;       ;        ;     ;        ; 
;   ;      ;      ;   ;    ;    ;      ;   ;         ;   ;  ;   ;  ;; ;;          ;   ;  ;        ;     ;    ;   ; 
;   ;;;;;;  ;;;;   ;;;;    ;     ;;;;   ;;;           ;;;;  ;   ;   ;;;;           ;;;   ;      ;;;;;   ;;;   ;;;  
;                                                                                                                  
;                                                                                                                  
;                                                                                                                  



;;;;;; Making data for the generators;;;;;;;


(define (different-endpoints? leaf1 leaf2)
  (not (or (= (leaf-a leaf1) (leaf-a leaf2))
           (= (leaf-a leaf1) (leaf-b leaf2))
           (= (leaf-b leaf1) (leaf-a leaf2))
           (= (leaf-b leaf1) (leaf-b leaf2)))))


(define (leaf-endpoints-not-in-orbit? leaf orbit)
  (define (iter remaining-orbit)
    (cond [(null? remaining-orbit)
           #t]
          [(and (different-endpoints? leaf (leaf-a (car remaining-orbit)))
                (different-endpoints? leaf (leaf-b (car remaining-orbit))))
           (iter (cdr remaining-orbit))]))
  (iter orbit))

  
(define (gen-crit-portrait d orbit)
  (define (crits-from-a-orbit d orbit)
    (define (iter left-of-orbit crits)
      (cond
        [(null? left-of-orbit)
         crits]
        [else (iter (cdr left-of-orbit)
                    (append crits
                            (filter (lambda (crit) (not (leaf-cross-collection? crit orbit)))
                                    (gen-deg2-crit-chords-from-pt d (caar left-of-orbit) orbit))))]))
    (iter orbit '()))
  (define crit-portraits (filter (lambda (crits) (and (not (leaves-cross? (first crits) (second crits)))
                                                      (different-endpoints? (first crits) (second crits))))
                                 (combinations (crits-from-a-orbit d orbit) 2)))
  (if (null? crit-portraits)
      '()
      (car crit-portraits)))


(define lam-period-3-orbits (filter (lambda (orbit) (laminational? 3 (car orbit))) period-3-orbits))
(define crits-of-lam-period-3-orbits (map (lambda (orbit) (gen-crit-portrait 3 orbit)) lam-period-3-orbits))
(define d-nary-crits-of-lam-period-3-orbits
  (map (lambda (crits)
         (list (make-leaf (d-nary-periodic-point 3 (leaf-a (first crits)))
                          (d-nary-preperiodic-point 3 (leaf-b (first crits))))
               (make-leaf (d-nary-periodic-point 3 (leaf-a (second crits)))
                          (d-nary-preperiodic-point 3 (leaf-b (second crits))))))
       crits-of-lam-period-3-orbits))

(define (Orbits-cross? orbit1 orbit2)
  (define (iter orbit1 orbit2)
    (cond [(null? orbit1) #f]
          [(leaf-cross-collection? (car orbit1) orbit2) #t]
          [else (iter (cdr orbit1) orbit2)]))
  (iter orbit1 orbit2))


;;;; Now the generating files ;;;;;

; Makes json given period 3 leaf orbit and critical portrait
(define (make-period-3-leaf-orbit-with-crits-json d p orbit crits)
  (define d-nary-orbit (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit))
  (define leaf1 (first d-nary-orbit))
  (define leaf2 (second d-nary-orbit))
  (define leaf3 (third d-nary-orbit))
  (define d-nary-crits (map (lambda (crit)
                              (make-leaf (d-nary-periodic-point 3 (leaf-a crit))
                                         (d-nary-preperiodic-point 3 (leaf-b crit))))
                              crits))
  (define crit1 (map point-to-string (first d-nary-crits)))
  (define crit2 (map point-to-string (second d-nary-crits)))

  (define filename (string-append (leaf-a leaf1) (leaf-b leaf1) ".json"))

  (with-output-to-file filename (lambda ()
                                  (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit of Minor ~a-~a.\",
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
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
                                          p d
                                          (leaf-a leaf1) (leaf-b leaf1)
                                          (leaf-a crit1) (leaf-b crit1) (leaf-a crit1)
                                          (leaf-a crit2) (leaf-b crit2) (leaf-a crit2) 
                                          (leaf-a leaf1) (leaf-b leaf1) (leaf-a leaf2) (leaf-b leaf2) (leaf-a leaf3) (leaf-b leaf3)
                                          (leaf-a leaf1) (leaf-a leaf2) (leaf-a leaf3)))))

; Makes all jsons given period 3 leaf orbits and their critical portraits
(define (make-period-3-leaf-orbit-with-crits-jsons d p orbits crits)
  (define (iter orbits crits)
    (cond [(null? orbits)
           (pretty-print "Done!")]
          [else 
           (make-period-3-leaf-orbit-with-crits-json d p (car orbits) (car crits))
           (iter (cdr orbits) (cdr crits))]))
  (iter orbits crits))

; Writes tex commands for figures for period 3 leaf orbits and critical leaves to "temp.txt"
(define (make-latex-period-3-leaf-orbits-with-crit d orbits)
  (define d-naryo1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits)))
  (define d-naryo2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits)))
  (define d-naryo3 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (third orbits)))
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3WithCrits/~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3WithCrits/~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3WithCrits/~a~a.png}
\\end{subfigure}
\\end{figure}"
           (first (first d-naryo1)) (second (first d-naryo1)) 
           (first (first d-naryo2)) (second (first d-naryo2))
           (first (first d-naryo3)) (second (first d-naryo3)))
  (close-output-port out))

(define (make-period-3-leaf-orbit-with-crit-latex d p orbits)
  (define (iter orbits)
    (pretty-print orbits)
    (cond [(null? orbits)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbits))
           (make-latex-period-3-leaf-orbits-with-crit d (list (first orbits) (first orbits) (first orbits)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbits))
           (make-latex-period-3-leaf-orbits-with-crit d (list (first orbits) (second orbits) (second orbits)))
           (pretty-print "c3: Done!")]
          [else 
           (make-latex-period-3-leaf-orbits-with-crit d orbits)
           (iter (cdddr orbits))]))
  (iter orbits))


;                                                                                                                          
;                                                                                                                          
;                    ;;;               ;             ;;;                                    ;          ;                   
;  ;;   ;;             ;      ;        ;               ;                      ;;;           ;          ;      ;            
;  ;;   ;;             ;      ;                        ;                     ;   ;          ;                 ;            
;  ;;; ;;;  ;    ;     ;    ;;;;;;   ;;;    ; ;;;      ;     ;;;;           ;     ;  ;;;;;  ; ;;;    ;;;    ;;;;;;   ;;;;  
;  ; ; ; ;  ;    ;     ;      ;        ;    ;;  ;      ;     ;  ;;          ;     ;  ;;     ;;  ;      ;      ;     ;    ; 
;  ; ; ; ;  ;    ;     ;      ;        ;    ;    ;     ;    ;    ;          ;     ;  ;      ;    ;     ;      ;     ;      
;  ;  ;  ;  ;    ;     ;      ;        ;    ;    ;     ;    ;;;;;;          ;     ;  ;      ;    ;     ;      ;     ;;;    
;  ;  ;  ;  ;    ;     ;      ;        ;    ;    ;     ;    ;               ;     ;  ;      ;    ;     ;      ;        ;;; 
;  ;     ;  ;    ;     ;      ;        ;    ;    ;     ;    ;               ;     ;  ;      ;    ;     ;      ;          ; 
;  ;     ;  ;   ;;     ;      ;        ;    ;;  ;      ;     ;               ;   ;   ;      ;;  ;      ;      ;     ;    ; 
;  ;     ;   ;;; ;      ;;;    ;;;  ;;;;;;; ; ;;;       ;;;  ;;;;;            ;;;    ;      ; ;;;   ;;;;;;;    ;;;   ;;;;  
;                                           ;                                                                              
;                                           ;                                                                              
;                                           ;                                                                              
;                                                                                                                          

; Makes json given two period 3 leaf orbits
(define (make-period-3-leaf-orbits-json d p orbit1 orbit2)
  (define d-nary-orbit1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit1))
   (define d-nary-orbit2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit2))
  (define leaf1o1 (first d-nary-orbit1))
  (define leaf2o1 (second d-nary-orbit1))
  (define leaf3o1 (third d-nary-orbit1))
  (define leaf1o2 (first d-nary-orbit2))
  (define leaf2o2 (second d-nary-orbit2))
  (define leaf3o2 (third d-nary-orbit2))
  (define filename (string-append (leaf-a leaf1o1) (leaf-b leaf1o1) (leaf-a leaf1o2) (leaf-b leaf1o2) ".json"))

  (with-output-to-file filename (lambda ()
                                  (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit of Minors ~a-~a and ~a-~a.\",
  \"branches\": [
    {
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
{
      \"chord\": [\"0\", \"0\"],
      \"endpoints\": [\"0\"]
    },
  ],

\"leaves\": [
	{\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
	{\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
                                          p d
                                          (leaf-a leaf1o1) (leaf-b leaf1o1)
                                          (leaf-a leaf1o2) (leaf-b leaf1o2)
                                          (leaf-a leaf1o1) (leaf-b leaf1o1)
                                          (leaf-a leaf2o1) (leaf-b leaf2o1)
                                          (leaf-a leaf3o1) (leaf-b leaf3o1)
                                          (leaf-a leaf1o1)
                                          (leaf-a leaf2o1)
                                          (leaf-a leaf3o1)
                                          (leaf-a leaf1o2) (leaf-b leaf1o2)
                                          (leaf-a leaf2o2) (leaf-b leaf2o2)
                                          (leaf-a leaf3o2) (leaf-b leaf3o2)
                                          (leaf-a leaf1o2)
                                          (leaf-a leaf2o2)
                                          (leaf-a leaf3o2)))))

(define (make-period-3-leaf-orbits-jsons d p orbit-pairs)
  (define (iter orbit-pairs)
    (cond [(null? orbit-pairs)
           (pretty-print "Done!")]
          [else 
           (make-period-3-leaf-orbits-json d p (first (first orbit-pairs)) (second (first orbit-pairs)))
           (iter (cdr orbit-pairs))]))
  (iter orbit-pairs))


; Makes latex file file for two non-crossing period 3 orbits
(define (make-latex-period-3-leaves-orbits d orbit-pairs)
  (define orbits1 (first orbit-pairs))
  (define orbits2 (second orbit-pairs))
  (define orbits3 (third orbit-pairs))
  (define d-naryp1o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits1)))
  (define d-naryp1o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits1)))
  (define d-naryp2o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                 (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits2)))
  (define d-naryp2o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits2)))
  (define d-naryp3o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits3)))
  (define d-naryp3o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits3)))
  
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleOrbits/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleOrbits/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleOrbits/~a~a~a~a.png}
\\end{subfigure}
\\end{figure}"
           (first (first d-naryp1o1)) (second (first d-naryp1o1)) (first (first d-naryp1o2)) (second (first d-naryp1o2)) 
           (first (first d-naryp2o1)) (second (first d-naryp2o1)) (first (first d-naryp2o2)) (second (first d-naryp2o2)) 
           (first (first d-naryp3o1)) (second (first d-naryp3o1)) (first (first d-naryp3o2)) (second (first d-naryp3o2))) 
  (close-output-port out))

(define (make-period-3-leaves-orbits-latex d p orbit-pairs)
  (define (iter orbit-pairs)
    (cond [(null? orbit-pairs)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbit-pairs))
           (make-latex-period-3-leaves-orbits d (list (first orbit-pairs) (first orbit-pairs) (first orbit-pairs)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbit-pairs))
           (make-latex-period-3-leaves-orbits d (list (first orbit-pairs) (second orbit-pairs) (second orbit-pairs)))
           (pretty-print "c3: Done!")]
          [else 
           (make-latex-period-3-leaves-orbits d orbit-pairs)
           (iter (cdddr orbit-pairs))]))
  (iter orbit-pairs))

; Makes json given two period 3 leaf orbits and critical chords
(define (make-period-3-leaf-orbits-with-crits-json d p orbit1 orbit2 crits)
  (define d-nary-orbit1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit1))
   (define d-nary-orbit2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                      (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                            orbit2))
  (define leaf1o1 (first d-nary-orbit1))
  (define leaf2o1 (second d-nary-orbit1))
  (define leaf3o1 (third d-nary-orbit1))
  (define leaf1o2 (first d-nary-orbit2))
  (define leaf2o2 (second d-nary-orbit2))
  (define leaf3o2 (third d-nary-orbit2))
  (define d-nary-crits (map (lambda (crit)
                              (make-leaf (d-nary-periodic-point 3 (leaf-a crit))
                                         (d-nary-preperiodic-point 3 (leaf-b crit))))
                              crits))
  (define crit1 (map point-to-string (first d-nary-crits)))
  (define crit2 (map point-to-string (second d-nary-crits)))
  (define filename (string-append (leaf-a leaf1o1) (leaf-b leaf1o1) (leaf-a leaf1o2) (leaf-b leaf1o2) ".json"))

  (with-output-to-file filename (lambda ()
                                  (printf "{
  \"name\": \"Period ~a Leaf\",
  \"base\": ~a,
  \"description\": \"Orbit of Minors ~a-~a and ~a-~a.\",
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
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"green\", \"fillColor\": \"green\"}},
	{\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\", \"~a\"]},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"~a\"], \"settings\": {\"strokeWidth\": 5,\"strokeColor\":\"orange\", \"fillColor\": \"orange\"}},
        {\"points\": [\"_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"1_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},
        {\"points\": [\"2_0\"], \"settings\": {\"strokeWidth\": 6,\"strokeColor\":\"black\", \"fillColor\": \"black\"}},

]
}"
                                          p d
                                          (leaf-a leaf1o1) (leaf-b leaf1o1)
                                          (leaf-a leaf1o2) (leaf-b leaf1o2)
                                          (leaf-a crit1) (leaf-b crit1) (leaf-a crit1)
                                          (leaf-a crit2) (leaf-b crit2) (leaf-a crit2) 
                                          (leaf-a leaf1o1) (leaf-b leaf1o1)
                                          (leaf-a leaf2o1) (leaf-b leaf2o1)
                                          (leaf-a leaf3o1) (leaf-b leaf3o1)
                                          (leaf-a leaf1o1)
                                          (leaf-a leaf2o1)
                                          (leaf-a leaf3o1)
                                          (leaf-a leaf1o2) (leaf-b leaf1o2)
                                          (leaf-a leaf2o2) (leaf-b leaf2o2)
                                          (leaf-a leaf3o2) (leaf-b leaf3o2)
                                          (leaf-a leaf1o2)
                                          (leaf-a leaf2o2)
                                          (leaf-a leaf3o2)))))

(define (make-period-3-leaf-orbits-with-crits-jsons d p orbit-pairs crits)
  (define (iter orbit-pairs crits)
    (cond [(null? orbit-pairs)
           (pretty-print "Done!")]
          [else 
           (make-period-3-leaf-orbits-with-crits-json d p (first (first orbit-pairs)) (second (first orbit-pairs)) (first crits))
           (iter (cdr orbit-pairs) (cdr crits))]))
  (iter orbit-pairs crits))

; Makes latex file file for two non-crossing period 3 orbits
(define (make-latex-period-3-leaves-orbits-with-crits d orbit-pairs)
  (define orbits1 (first orbit-pairs))
  (define orbits2 (second orbit-pairs))
  (define orbits3 (third orbit-pairs))
  (define d-naryp1o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits1)))
  (define d-naryp1o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits1)))
  (define d-naryp2o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                 (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits2)))
  (define d-naryp2o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits2)))
  (define d-naryp3o1 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (first orbits3)))
  (define d-naryp3o2 (map (lambda (leaf) (make-leaf (point-to-string (d-nary-periodic-point d (leaf-a leaf)))
                                                  (point-to-string (d-nary-periodic-point d (leaf-b leaf)))))
                        (second orbits3)))
  
  (define out (open-output-file "temp.tex" #:exists 'append))
  (fprintf out "\n
\\begin{figure}[!htbp]
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleWithCrits/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleWithCrits/~a~a~a~a.png}
\\end{subfigure}
\\begin{subfigure}[b]{0.32\\textwidth}
    \\centering
    \\includegraphics[width=\\textwidth]{Period3CompatibleWithCrits/~a~a~a~a.png}
\\end{subfigure}
\\end{figure}"
           (first (first d-naryp1o1)) (second (first d-naryp1o1)) (first (first d-naryp1o2)) (second (first d-naryp1o2)) 
           (first (first d-naryp2o1)) (second (first d-naryp2o1)) (first (first d-naryp2o2)) (second (first d-naryp2o2)) 
           (first (first d-naryp3o1)) (second (first d-naryp3o1)) (first (first d-naryp3o2)) (second (first d-naryp3o2))) 
  (close-output-port out))

(define (make-period-3-leaves-orbits-latex-with-crits d p orbit-pairs)
  (define (iter orbit-pairs)
    (cond [(null? orbit-pairs)
           (pretty-print "c1: Done!")]
          [(null? (cdr orbit-pairs))
           (make-latex-period-3-leaves-orbits-with-crits d (list (first orbit-pairs) (first orbit-pairs) (first orbit-pairs)))
           (pretty-print "c2: Done!")]
          [(null? (cddr orbit-pairs))
           (make-latex-period-3-leaves-orbits-with-crits d (list (first orbit-pairs) (second orbit-pairs) (second orbit-pairs)))
           (pretty-print "c3: Done!")]
          [else 
           (make-latex-period-3-leaves-orbits-with-crits d orbit-pairs)
           (iter (cdddr orbit-pairs))]))
  (iter orbit-pairs))


;;;;;; Making data for the generators ;;;;;;;

(define orbit-comb (combinations lam-period-3-orbits 2))
(define non-crossing-orbits (filter (lambda (orbits)
                                      (not (Orbits-cross? (first orbits) (second orbits)))) orbit-comb))
(define (orbits-dont-share-endpoint? orbit1 orbit2)
  (null? (set-intersect (flatten orbit1) (flatten orbit2))))
(define non-crossing-orbits-dif-endpoints (filter (lambda (orbits) (orbits-dont-share-endpoint? (first orbits) (second orbits))) non-crossing-orbits))
(define (comb-has-crit-chords? d comb)
         (not (null? (gen-crit-portrait d (append (first comb) (second comb))))))
(define non-crossing-combs-with-crits
  (filter (lambda (comb) (comb-has-crit-chords? 3 comb)) non-crossing-orbits))
(define the-crits-of-non-crossing-combs-with-crits
  (filter (lambda (x) (not (null? x))) (map (lambda (comb) (gen-crit-portrait 3 (append (first comb) (second comb)))) non-crossing-orbits)))
(define non-crossing-combs-with-crits-dif-endpoints
  (filter (lambda (comb) (comb-has-crit-chords? 3 comb)) non-crossing-orbits-dif-endpoints))
(define the-crits-of-non-crossing-combs-with-crits-dif-endpoints
  (filter (lambda (x) (not (null? x))) (map (lambda (comb) (gen-crit-portrait 3 (append (first comb) (second comb)))) non-crossing-orbits-dif-endpoints)))


  
  
    

