; notes
;
; strings are zero indexed in substring
;  0 1 2 3
;  a b c d
; > (substring "abcd" 0 2)
; "ab"
; > (substring "abcd" 2)
; "cd"

; for a pair you can get first item by (car pr) and second by (cdr pr)
; for a list that gets you 'first-item '(rest of list)
; you can however do car and cadr or simply first and second

; returns #t if first half and second half of str are equal
(define (invalid? str) 
  (define middle (floor (/ (string-length str) 2)))
  (equal? (substring str 0 middle) (substring str middle)))

; makes the list of numbers '(from from+1 ... to)
(define (build-range from to) (build-list (- (add1 to) from) (lambda (x) (+ x from))))

; returns a list of the invalid numbers in (build-range from to)
(define (find-invalid from to) 
  (filter (lambda (x) (invalid? (number->string x))) (build-range from to)))

; goes reads fname and returns a list of two-item lists that define input ranges
(define (parse-inputfile fname) 
  ; reads input file to something like '("11-22" "95-115" ...)
  (define range-strs (string-split (string-trim (file->string fname) "\n") ","))
  (map (lambda (x) (map string->number x))
    ; maps "11-22" to '("11" "22") across list
    (map (lambda (x) (string-split x "-")) range-strs)))

(define (validate-file fname)
  (define input (parse-inputfile fname))
  (define invalid-numbers 
    (map (lambda (x) (find-invalid (first x) (second x))) input))
  ; sum inner lists then sum outer list
  (foldl + 0 (map (lambda (x) (foldl + 0 x)) invalid-numbers)))

(validate-file "input.txt")
; 22062284697

; for part 2 we basically need a more general invalid? function
(define str "12121")

; partitions a string into its n-character components
; "1231231" -> '("12" "31" "23" "1")
(define (partition str n) 
  (define (part str lst) 
    (if (<= (string-length str) n) 
        (cons str lst)
        (part (substring str n) (cons (substring str 0 n) lst))))
  (reverse (part str '())))

(define (invalidn? str n) 
  (define partitioned (partition str n))
  (andmap (lambda (x) (equal? x (first partitioned))) partitioned))

(define (invalid2? str) 
  (ormap (lambda (n) (invalidn? str n)) 
         ; this is just a list of the numbers '(1 2 ... strlen/2)
         (build-list (floor (/ (string-length str) 2)) add1)))

(define (find-invalid2 from to) 
  (filter (lambda (x) (invalid2? (number->string x))) (build-range from to)))

(define (validate-file2 fname)
  (define input (parse-inputfile fname))
  (define invalid-numbers 
    (map (lambda (x) (find-invalid2 (first x) (second x))) input))
  ; sum inner lists then sum outer list
  (foldl + 0 (map (lambda (x) (foldl + 0 x)) invalid-numbers)))

; this isn't super fast (ie I can tell time passes between input and result
(validate-file2 "input.txt")
