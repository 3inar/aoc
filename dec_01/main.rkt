#lang racket

; some notes:
; (substring str start end) is 0-indexed; end is optional
; (substring "L21" 1) 

; check prefix
;(string-prefix? "L21" "L")

; converts a string to a number
; (string->number "21")

; define a function
; (define (double x) (* 2 x))

; define a function that returns a function
; (define (multby n) (lambda (x) (* n x)))
; > ((multby 3) 3)
; 9
; > ((multby 3) 4)
; 12

; assumes well-behaved input
(define (parse-rotation str) 
  ; convert last bit to number
  (define distance (string->number (substring str 1)))
  ; direction is - if string starts with L, + otherwise
  (define direction (if (string-prefix? str "L") - +))
  ; return a function that either adds or subtracts distance mod 100
  (lambda (x) (modulo (direction x distance) 100)))

; makes the rotate call nicer
(define (rotate rot origin) ((parse-rotation rot) origin))

; ends up at zero
;(define testlist (list "L21" "R21" "R50"))
; ends up at 50, never goes to zero
;(define testlist (list "L21" "R21"))

; if you start this at position zero you must pass 1 for the zeros argument
(define (apply-rotations rotation-list origin zeros) 
  (define new-position (rotate (car rotation-list) origin))
  (define new-zeros (if (equal? new-position 0) (+ zeros 1) zeros))
  
  ; if end of list return position and number of zeros encountered
  ; otherwise recurse
  (if (empty? (cdr rotation-list))
      (cons new-position new-zeros)
      (apply-rotations (cdr rotation-list) new-position new-zeros)))

; should count 3 zeros according to the instructions
;(define input (file->lines "example.txt"))
;(apply-rotations input 50 0)

(define input (file->lines "input.txt"))
(apply-rotations input 50 0)
; '(47 . 1180)

; part 2 requires that we count each time we PASS zero also
; I will do this in the laziest way possible

; takes something like "L15" and returns a list of 15 entries of "L1"
(define (expand-rotation rot) 
  (define distance (string->number (substring rot 1)))
  (define single-step (string-append (substring rot 0 1) "1"))
  ; this is probably a crime against good taste
  (build-list distance (lambda (n) single-step)))

(define (expand-rotation-list lst) 
  (foldr append '() (map expand-rotation lst)))

(define testlist (list "L3" "R2" "M2"))
(expand-rotation-list testlist)

; should count 6 zeros according to the instructions
;(define input (file->lines "example.txt"))
;(apply-rotations (expand-rotation-list input) 50 0)

(define input (file->lines "input.txt"))
(apply-rotations (expand-rotation-list input) 50 0)
; '(47 . 6892)
