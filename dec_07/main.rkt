(define diagram (file->lines "input.txt"))

; in part 2 a beam isn't split in two but goes to the left or to the right.
; the task is to count the number of different paths the beam could take.
; I had a rash idea to count by recursion but it was too demanding, the problem 
; seems too large. instead I will modify my original function to just count how 
; many unique paths go through a given tile and propagate this downward
;
; ie ....2...    ->     ....2...
;    ....^...           ...2^2..
; which requires me to keep track of cases where two paths split onto one another
;
; ie ...32...    ->     ...32...
;    ....^...           ...5^2..
;
; or ...321..    ->     ...321..
;    ....^...           ...5^3..

(define (process-line cur nxt updated num-splits)
  (define (source? ch) 
    (equal? ch #\S))
  (define (beam? ch)
    (number? ch))
  (define (dot? ch)
    (equal? ch #\.))
  (define (split? ch)
    (equal? ch #\^))
  (cond
    [(empty? cur) (list (reverse updated) num-splits)]
    [(source? (car cur))
     ; one beam goes out of the source. I assume no splitter directly below source
     (process-line (cdr cur) (cdr nxt) (cons 1 updated) num-splits)]
    [(not (beam? (car cur))) 
     ; empty tile: do nothing
     (process-line (cdr cur) (cdr nxt) (cons (car nxt) updated) num-splits)]
    [(and (beam? (car cur)) (dot? (car nxt)))
     ; if some number of beams go to an empty tile, propagate this number down
     (process-line (cdr cur) (cdr nxt) (cons (car cur) updated) num-splits)]
    [(and (beam? (car cur)) (split? (car nxt)))
     ; this is the tricky case; need to figure out what's "next" to the current 
     ; set of beams
     (let* ([cnum (car cur)] 
            [rnum (if (beam? (car updated)) (car updated) 0)]
            [lnum (if (beam? (cadr cur)) (cadr cur) 0)]
            [split-beam (list (+ cnum lnum) #\^ (+ cnum rnum))])
     (process-line (cddr cur) (cddr nxt) (append split-beam (cdr updated)) (add1 num-splits)))]
    [else (error "should not happen")]))

(define (read-diagram diagram new-diagram num-splits)
  (if (empty? diagram)
      (list new-diagram num-splits)
  (let* ([current-lst (car new-diagram)]
        [next-lst (string->list (car diagram))]
        [updated-line (process-line current-lst next-lst '() 0)])
    (read-diagram (cdr diagram)
                  (cons (first updated-line) new-diagram)
                  (+ num-splits (second updated-line))))))

(define res (read-diagram (cdr diagram) (list (string->list (car diagram))) 0))
(foldl + 0 (filter number? (caar res)))

