; conjecture: it's enough to do this in a greedy way.
;   i. remove the last digit from the list, find the largest digit a
;   ii. remove everuthing up to and including a from the original list and find 
;       the largest digit b
;   iii. ab should be the largest possible 2 digit number
; since order matters the last digit can't be in the 10 position but otherwise 
; you want to maximize this since no smaller number can make anything larger no 
; matter what you choose for the 1 position
;
; part 2 is just the same kind of thinking  but in step 1 exclude the last 
; 11 digits, step 2 exclude the last 10, etc. etc.

; this is from day 2, splits a string into n-character strings
(define (partition str n) 
  (define (part str lst) 
    (if (<= (string-length str) n) 
        (cons str lst)
        (part (substring str n) (cons (substring str 0 n) lst))))
  (reverse (part str '())))

; maximize jolts in bank (str) over n batteries
(define (joltmaxx str n)
  ; splits string into list of digits
  (define bank (map string->number (partition str 1)))

  ; a recursive maximizer 
  (define (jmaxx lst n solution) 
    (define maxest (argmax values (take lst (- (length lst) (sub1 n)))))
    (define new-solution (+ (* solution 10) maxest))
    (if (equal? n 1) 
        new-solution
        (jmaxx (cdr (member maxest lst)) (sub1 n) new-solution)))
  (jmaxx bank n 0))

  ; remove last digit, find max
  ; (define tens (argmax values (cdr (reverse lst))))
  ; remove first part of list up to and including tens; find max
  ; (define ones (argmax values (cdr (member tens lst))))
  ; (+ (* 10 tens) ones))

(define banks (file->lines "input.txt"))
(foldl + 0 (map (lambda (x) (joltmaxx x 12)) banks))


