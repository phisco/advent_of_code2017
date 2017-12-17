#lang racket

(define (step coord direction)
  (let  ((x (car coord))
         (y (cdr coord)))
    (let ((res (cond
      ((equal? direction "ne")  (cons (+ 1 x) (- y 1)))
      ((equal? direction "e") (cons (+ 1 x) y))
      ((equal? direction "se") (cons x (+ y 1)))
      ((equal? direction "sw") (cons (- x 1) (+ y 1)))
      ((equal? direction "w") (cons (- x 1) y))
      ((equal? direction "nw") (cons x (- y 1)))
      (#t (displayln "error"))
      )))
      res)))

;(step  '(0 . 0) "ne")

(define (translate direction)
  (cond
    ((equal? direction "n") "e")
    ((equal? direction "ne") "se")
    ((equal? direction "se") "sw")
    ((equal? direction "s") "w")
    ((equal? direction "sw") "nw")
    ((equal? direction "nw") "ne")
    )
)

;(abs(a.q - b.q) 
;          + abs(a.q + a.r - b.q - b.r)
;          + abs(a.r - b.r)) / 2

(define (distance coord1 coord2)
  (let ((aq (car coord1))
        (ar (cdr coord1))
        (bq (car coord2))
        (br (cdr coord2)))
    (/ (+ (abs (- aq bq)) (abs (- (+ aq ar) (+ bq br))) (abs (- ar br))) 2)))

;(distance (step '(0 . 0) "ne"))
;(distance '(1 . -1))

(define (countSteps coord dir)
  (if (eq? dir '())
      (distance coord '(0 . 0))
      (let ((translatedDirection (translate (car dir))))
                    (countSteps (step coord translatedDirection) (cdr dir))
	)
  ))

;(countSteps '(0 . 0) '("se" "sw" "se" "sw" "sw") )
;(distance  '(0 . 0) '(0 . 0))
;(countSteps '(0 . 0) '("ne" "ne" "sw" "sw"))

'(let ((in (read-line (open-input-file "/home/phisco/Github/code_calendar/11.txt"))))
  (let ((splitted (string-split in ",")))
    (countSteps '(0 . 0) splitted)
    )
  )

(define (findmax s d m) (let ((maximum (max m (distance '(0 . 0) s))))
                           (if (equal? d '())
                             maximum
                             (begin (display (car d)) (display s) (displayln maximum) (findmax (step s (translate (car d))) (cdr d) maximum ))
                             )))

(let ((in (read-line (open-input-file "/home/phisco/Github/code_calendar/11a.txt"))))
  (let ((splitted (string-split in ",")))
    (findmax '(0 . 0) splitted 0)
  ))

(findmax '(0 . 0) '("se" "sw" "se" "sw" "sw") 0)