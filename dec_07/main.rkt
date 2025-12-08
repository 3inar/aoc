(define diagram (file->lines "input.txt"))

(define (process-line cur nxt updated num-splits)
  (define (beam? ch)
    (or (equal? ch #\|) (equal? ch #\S)))
  (define (dot? ch)
    (equal? ch #\.))
  (define (split? ch)
    (equal? ch #\^))
  (define split-beam (list #\| #\^ #\|))
  ; simplest case: no beam on cur: next isn't modified
  ; simple case: (S or |) on cur,  not ^ on next: output | on next
  ; tricky case (not super): (S or |) on cur, ^ on next:
  ;  i.   remove the previous update to next
  ;  ii.  output |.| to next
  ;  iii. remove a char from cur and nxt(accounting for the extra character added to
  ;       next) -- assuming there are never two splitters in a row
  ; iv.   increment num splits,
  (cond
    [(empty? cur) (list (list->string (reverse updated)) num-splits)]
    [(not (beam? (car cur))) (process-line (cdr cur) (cdr nxt) (cons (car nxt) updated) num-splits)]
    [(and (beam? (car cur)) (dot? (car nxt)))
     (process-line (cdr cur) (cdr nxt) (cons #\| updated) num-splits)]
    [(and (beam? (car cur)) (split? (car nxt)))
     (process-line (cddr cur) (cddr nxt) (append split-beam (cdr updated)) (add1 num-splits))]
    [else (error "should not happen")]))

(define (read-diagram diagram new-diagram num-splits)
  (if (empty? diagram)
      (list new-diagram num-splits)
  (let* ([current-lst (string->list (car new-diagram))]
        [next-lst (string->list (car diagram))]
        [updated-line (process-line current-lst next-lst '() 0)])
    (read-diagram (cdr diagram)
                  (cons (first updated-line) new-diagram)
                  (+ num-splits (second updated-line))))))

(read-diagram (cdr diagram) (list (car diagram)) 0)

; in part 2 a beam isn't split in two but goes to the left or to the right.
; the task is to count the number of different paths the beam could take.
; seems like a recursion type problem (like everything else in a LISP I guess)
