; since the operators are just + and * it don't matter that I reverse the lines
(define (tokenize str)  (filter (lambda (x) (not (equal? x ""))) (string-split str " ")))
(define homework (map tokenize (reverse (file->lines "input.txt"))))

(define (solve hw) 
  (define (solver hw soln)
    (if (empty? (car hw))
        soln
        ; it's let* because let evaluates the assignments in isolation so you
        ; can't refer back
        (let* ([problem (map car hw)] 
               [operator (if (equal? (car problem) "*") * +)]
               [operands (map string->number (cdr problem))]
               [ident (if (equal? operator *) 1 0)]
               [solution (foldl operator ident operands)])
          (solver (map cdr hw) (cons solution soln)))))
  (solver hw '()))
(foldl + 0 (solve homework))
