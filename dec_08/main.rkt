; distance between two boxes
(define (d v1 v2)
  ; make vector from "123,456,78" type strings
  (define (s->l str)
    (map string->number (string-split str ",")))
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
  (define (split-pair s)
    (string-split s "-"))
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

(define pairs (distances (file->lines "example.txt") '()))
(apply * (take (sort (map length (make-n-connections pairs 10)) >) 3))
