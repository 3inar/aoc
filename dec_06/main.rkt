; since the operators are just + and * it don't matter that I reverse order of 
; the lines
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

; part 2 is a radically different way of reading the operands. here it does 
; matter if I reverse the line order 
(define homework
  (map (lambda (x) (filter (lambda (y) (not (equal? "" y))) (string-split x "")))
       (reverse (file->lines "input.txt"))))
(define operators
  (map (lambda (x) (if (equal? x "*") * +))
       (filter (lambda (y) (not (equal? y " "))) (car homework))))
(define raw-operands (reverse (cdr homework)))

; will return #f for (" " " " ... " ") ie the separator cols
(define (col->number col)
  (string->number (string-join (filter (lambda (x) (not (equal? x " "))) col) "")))

(define (raw->numbers raw) 
  (define (process-next raw nums) 
    (define next (col->number (map car raw)))
    (define nn (cons next nums))
    (if (= 1 (length (car raw)))
        nn
        (process-next (map cdr raw) nn)))
  (reverse (process-next raw '())))

(define nums (raw->numbers raw-operands))

(define (calculate ns ops res) 
  (define-values (current rst) (splitf-at ns number?))
  (define nres (foldl (car ops) (if (equal? * (car ops)) 1 0) current))
  (if (= (length ops) 1)
      (cons nres res)
      (calculate (cdr rst) (cdr ops) (cons nres res))))

(foldl + 0 (calculate nums operators '()))

