#lang racket
(require racket/set)

(define (findNeigh start neighs)
  (if (equal? neighs '(()))
      start
  (let* ((h (car neighs))
        (s1 (set-intersect start (car h)))
        (s2 (set-intersect start (cadr h)))
        (res (if (set-empty? s1)
      (if  (set-empty? s2)
           start
           (set-union start (caar neighs)))
      (set-union start (cadar neighs)))
)) (findNeigh res (cdr neighs)))))

(define (findGroup start neighs)
  (let ((found (findNeigh start neighs)))
    (if (set=? start found) start (findGroup found neighs))))
;neighs = ( ( (sideA), (sideB) ), ... )
;(findGroup (set 0) (list (cons (set 0)  (set 1)) (cons (set 1) (set 2))))

(define (transformLines in)
  (foldr (λ (line r) (cons (map (λ (l) (list->set (string-split l ", ")))
                                             (string-split line " <-> "))
                                        r)) '() in))
'(let ((in (file->lines "/home/phisco/Github/code_calendar/12a.txt")))
  (transformLines in)) 
'(let ((in (file->lines "/home/phisco/Github/code_calendar/12a.txt")))
  (length (set->list (findGroup (set "0") (transformLines in))))) ;288

(define (findSeparatedGroups neighs)
  (let ((world (getSetAll neighs)))
    (define (findSepGroups remaining ns found)
      (if (set-empty? remaining)
      found
      (let* ((rems (apply set-subtract (cons remaining found)) )
             (nextStart (set-first rems))
             (rem (set-remove rems nextStart)))
        (findSepGroups rem ns (cons (findGroup (set nextStart) ns) found)))
  ))
    (findSepGroups world neighs '() )
    ))

(define (getSetAll neighs)
  (define (f n found) (if (equal? n '(()))
                                 found
                                 (f (cdr n) (set-union (set-union (caar n) (cadar n) ) found)) ))
    (f neighs (set )))

'(let ((in (transformLines (file->lines "/home/phisco/Github/code_calendar/12a.txt"))))
  (getSetAll in))

(let ((in (transformLines (file->lines "/home/phisco/Github/code_calendar/12a.txt"))))
  (length (set->list (findSeparatedGroups in))))