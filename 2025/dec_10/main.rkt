#lang racket


; observation: pushing a button twice is pointless. that should be obvious
; for the case where you push it twice in a row but it is true whenever you
; push it. this is like flipping light switches: if you do it an odd number of
; times the light is on, if you do it an even number of times it is off. two
; presses of the same button adds two flips of the switch: no effect.

; observation: you could model this as a shortest path problem. eg starting at 
; vertex .... following the edge called (0,2) takes you to the vertex #.#.
; it's not relevant for part 1 where it's enough to just check the various 
; combinations of buttons (n choose 1) (n choose 2) ... until you get to the 
; right state but MY SUSPICION is that the joltages will be associated with
; costs of pressing certain buttons and then it becomes a weighted shortest 
; path problem


(require "../modules/helpers.rkt")
(require "machine.rkt")

(define (solve-machine m)
  (define nbuttons (length (machine-buttons m)))
  (define noop (make-list (length (machine-lights m)) #f))
  (define (bruteforce choices)
    (if (empty? choices) #f
      (let* ([choice (car choices)]
             [keypress (foldl press-button
                              noop
                              (map (lambda (b c) (if c b noop)) (machine-buttons m) choice))])
        (if (equal? keypress (machine-lights m))
            (bitsum choice)
            (bruteforce (cdr choices))))))
  (bruteforce (bitstrings nbuttons)))

(define machines (map line->machine (file->lines "example.txt")))
(for-each machine-print machines)
;(define machines (map line->machine (file->lines "input.txt")))
(apply + (map solve-machine machines))

; worst case we have buttons that only switch one light so the upper bound
; on a solution is the sum of joltages. 
;
; also no reason to search on if any one joltage goes beyond its target

; I see my conjecture about part 2 was wrong but I still think the graph idea 
; has merits; ie. we start on node '(0 0 0 0) and if we follow the edge '(0 3)
; we end up on node '(1 0 0 1). a breadth-first search will show us the fewest 
; keypresses to hit a certain end state; let's say the joltage req is '(2 1 0 1)
; and we could go like
;                '(0 3)                  '(0 1)
;  '(0 0 0 0) -----------> '(1 0 0 1) ------------>  '(2 1 0 1)
; my edges happen to be coded as '(#t #f #f #t) instead of '(0 3) but that kind
; of simplifies the task ---- actually it usees way too much memory like all my 
; 1st attempts
; 
; 

(define (follow-edge current-node edge) (map + current-node (bool->num edge)))

(define (dfs m) 
  (define target (machine-jolts m))
  (define soln (apply + (machine-jolts m)))
  (define edges (machine-buttons m))
  (define (dfs-helper current ct) 
    (cond [(>= ct soln) #f]
      [(ormap (compose negative? -) target current) #f]
      [(equal? current target) (set! soln ct) #t]
      [else (map (lambda (e) (dfs-helper (follow-edge current e) (add1 ct))) edges)]))
  (dfs-helper (make-list ((compose length machine-jolts) m) 0) 0)
  soln)


(define ln (length machines))

(define sols (map dfs machines))

(set! machines
      (map (lambda (m)
             (machine (machine-lights m) (map bool->num (machine-buttons m)) (machine-jolts m)))
           machines))

; if a button has 1 in the position where the jolt diagram has a 7 you can't
; press this button more than 7 times
(define (button-bounds m)
  (define buttons (machine-buttons m))
  (define jolts (machine-jolts m))
  (define mx (add1 (apply max jolts)))
  (map (lambda (button) (apply min (map (lambda (b j) (if (equal? 1 b) j mx)) button jolts)))
       buttons))

; some of these numbers are still like 10^20 so brute force not quite the thing
; but note that some buttons compete so you can't press both (1 0 1) and (1 1 0) 
; 7 times if the jolts are (7 7 7) since then you'd be at 14 in the first position
(map (lambda (x) (apply * x))(map button-bounds machines)) 

; let's try to make a branc and bound solver that don't use so much memory
(define bds (button-bounds m))



; don't work. I had no real hope it would
; (define (greedy m) 
;   (define target (machine-jolts m))
;   (define edges (machine-buttons m))
;   (define (nearest-edge cur edg) 
;     (define (dist v1 v2) (apply + (map (compose (lambda (x) (* x x)) -) v1 v2)))
;     (define nearest (argmin (lambda (e) (dist (follow-edge cur e) cur)) edg))
;     (follow-edge cur nearest))
;   (define (greedy-helper current ct) 
;     (cond 
;       [(ormap (compose negative? -) target current) #f]
;       [(equal? current target) ct]
;       [else (greedy-helper (nearest-edge current edges) (add1 ct))]))
;   (greedy-helper (make-list (length target) 0) 0))
; 
; (map greedy machines)


; could treat it as a linear algebra problem but solutions have to be 
; non-negative integers
;
;    
;                       (#t #t #f #f)
;                       (#t #f #t #f)
;          (a b c d e f)(#f #f #t #t)  = (3 5 4 7)
;                       (#f #f #t #f)
;                       (#f #t #f #t)
;                       (#f #f #f #t)
; minimize a+b+c+d+e+f s.t. a,..., f >= 0

;(require math/matrix)
;(define (edges->matrix edges) (list*->matrix (map bool->num edges)))
;(define EM (edges->matrix edges))
;(define J (list*->matrix (list (machine-jolts m))))
;(define Glued (matrix-augment (list (matrix-transpose EM)(matrix-transpose J))))
;
;  ; (map (compose list->matrix bool->num) buttonlist))
;(matrix-gauss-elim Glued #t #t)
;(matrix->list* (matrix-row-echelon Glued #t #t))

