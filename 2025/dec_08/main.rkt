#lang racket

; "1,2,3" -> '(1 2 3)
(define (s->l str)
  (map string->number (string-split str ",")))

; "xx-yy" -> '("xx" "yy")
(define (split-pair s)
  (string-split s "-"))

; distance between two boxes
(define (d v1 v2)
  (define (square x)
    (* x x))
  (sqrt (apply + (map square (map - (s->l v1) (s->l v2))))))

; builds every pair from-to and calculates the distance between them
(define (distances bx reslist)
  (define (box< a b) (< (cdr a) (cdr b)))
  (if (not (empty? bx))
      (distances (cdr bx)
                (cons (map (lambda (x) (cons (string-join (list (car bx) x) "-") (d (car bx) x)))
                           (cdr bx))
                      reslist))
      (sort (apply append reslist) box<)))

(define (add-connection pair circuits)
  (define (in-circuit? pr circ)
    (ormap values (map (lambda (x) (string-contains? pr x)) circ)))
  (define-values (in out) (partition (lambda (x) (in-circuit? pair x)) circuits))
  (if (empty? in)
      (cons (split-pair pair) circuits)
      (cons (append (split-pair pair) (apply append in)) out)))

(define (make-n-connections dst n) 
  (define (connector dst circs ct) 
    (if (equal? ct n) 
      circs
      (connector (cdr dst) (add-connection (caar dst) circs) (add1 ct))))
  (map remove-duplicates (connector dst '() 0)))


; for part 2 we can use add-connection repeatedly until there is only one circuit
(define (make-complete-circuit dst boxes)
  (define (connector dst circs added-last) 
    (if (equal? (length circs) 1) 
      added-last
      (connector (cdr dst) (add-connection (caar dst) circs) (caar dst))))
  (connector dst (map list boxes) "not a real pair"))

(define boxes (file->lines "input.txt"))
(define pairs (distances boxes '()))

; p1
; (apply * (take (sort (map length (make-n-connections pairs 10)) >) 3))

(apply * (map first (map s->l (split-pair (make-complete-circuit pairs boxes)))))
