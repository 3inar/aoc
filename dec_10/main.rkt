#lang racket

(require "../modules/helpers.rkt")

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


; baby's first struct. probably would have been a big help on day 9 where I 
; had to do a lot of (first (second )) type access which is  not easy to read
; nor write
(struct machine (lights buttons jolts))

(define (line->machine ln)
  (define split (string-split ln " "))
  (define lights
    (let ([raw-lights (car split)])
      (map (lambda (x) (equal? x #\#)) (string->list (remove-parens raw-lights)))))
  (define jolts
    (let ([raw-jolts ((compose car reverse) split)]) ((compose csv->nums remove-parens) raw-jolts)))
  (define (button->bool b num-lights)
    (define idx ((compose csv->nums remove-parens) b))
    (map (lambda (id) (if (member id idx) #t #f)) (range num-lights)))
  (define buttons
    (map (lambda (b) (button->bool b (length lights))) ((compose cdr reverse cdr) split)))
  (machine lights buttons jolts))


; counts the number of #t in a list: '(#t #t #f) -> 2
(define (bitsum bstr) (apply + (map (lambda (x) (if x 1 0)) bstr)))

; generates all bit strings of length n sorted by number of bits set
(define (bitstrings n)
  (define (addbit bit lst) (map (lambda (x) (cons bit x)) lst))
  (define (builder lst ct) 
    (if (equal? ct n)
      lst
      (builder (append (addbit #t lst) (addbit #f lst)) (add1 ct))))
  (sort (builder (list '(#t) '(#f)) 1) (lambda (a b) (< (bitsum a) (bitsum b)))))

(define (press-button machinebits buttonbits)
  (map xor buttonbits machinebits))

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

(define machines (map line->machine (file->lines "input.txt")))
(apply + (map solve-machine machines))

