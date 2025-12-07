; this one seems very straight-forward; you just have to check some numbers 
; against some ranges 

(define (in-range? x from to) (and (>= x from) (<= x to)))

(define database (file->lines "input.txt"))
(define food-ids (map string->number (cdr (member "" database)))) ; extract what's after the "" entry
(define ranges
  (map (lambda (x) (let ([rangestrings (string-split x "-")]) (map string->number rangestrings)))
       (takef database (lambda (entry) (not (equal? entry ""))))))

(define checks (map (lambda (id) (map (lambda (rg) (in-range? id (first rg) (second rg))) ranges)) food-ids))
(define valid (map (lambda (check) (ormap values check)) checks))
(foldl (lambda (x acc) (+ (if x 1 0) acc)) 0 valid)
