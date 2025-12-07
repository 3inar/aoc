; this one seems very straight-forward; you just have to check some numbers 
; against some ranges 

(define (in-range? x rg) (and (>= x (first rg)) (<= x (second rg))))

(define database (file->lines "input.txt"))
(define food-ids (map string->number (cdr (member "" database)))) ; extract what's after the "" entry
(define ranges
  (map (lambda (x) (let ([rangestrings (string-split x "-")]) (map string->number rangestrings)))
       (takef database (lambda (entry) (not (equal? entry ""))))))

(define checks (map (lambda (id) (map (lambda (rg) (in-range? id rg)) ranges)) food-ids))
(define valid (map (lambda (check) (ormap values check)) checks))
(foldl (lambda (x acc) (+ (if x 1 0) acc)) 0 valid)

; part 2 is to count the ids considered valid in the union of separate ranges
; I was hoping I could just do this the lazy way and add every unique number to 
; a list but when you inspect the numbers concerned it seems a bad idea

; note to self: could have made something like (struct interval (from to)) and 
; you get something like interval-from and interval-to to get those fields. just 
; sticking to plain lists for now
(define (range-in-range? r1 r2) (or (in-range? (first r1) r2) (in-range? (second r1) r2)))
(define (overlaps? r1 r2) (or (range-in-range? r1 r2) (range-in-range? r2 r1)))
(define (range-length r) (- (add1 (second r)) (first r)))

; assumes the items in lst together make a contiguous region 
(define (join-all lst) 
  (define flatlist (flatten lst))
  (list (argmin values flatlist) (argmax values flatlist)))

; reduces a list of ranges to an equivalent list of disjoint ranges.
; I consider something like [1 3] [4 6] disjoint here because the purpose 
; is simply to not double-count anything
(define (reduce lst disjt) 
  (define candidate (car lst))
  ; split the disjoint set into the ranges that overlaps with candidate and those
  ; that don't
  (define-values (overlap no-overlap) (partition (lambda (x) (overlaps? x candidate)) disjt))
  ; join the overlapping sets
  (define union (join-all (cons candidate overlap)))
  (if (= 1 (length lst))
      (cons union no-overlap)
      (reduce (cdr lst) (cons union no-overlap))))

(define (measure-fresh lst) 
  (define reduced (reduce lst '()))
  (define lengths (map range-length reduced))
  (foldl + 0 lengths))

(measure-fresh ranges)
