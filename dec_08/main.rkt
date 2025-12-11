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


(define uniqueboxes (remove-duplicates (apply append (map split-pair edges))))
(map (lambda (bx)
       (display bx)
       (map (lambda (circ)
              (if (in-circuit? bx circ)
                  (display "X")
                  (display "")))
            circuits)
       (newline))
     uniqueboxes)
; seems to be only 849 unique nodes in edges list so why are there 1048 that 
; participate in circuits? clearly something is wrong in take-circuit. should 
; double check if some circuits have nodes that are in other circuits why would 
; this happen for big input but not small??
(length uniqueboxes)
; 849
 (apply + (map length (detect-circuits edges)))
; 1048
; ( (take (sort (map length (detect-circuits edges)) >) 3))

(apply * (take (sort (map length (detect-circuits edges)) >) 3))
; right ans 67488

; finds n smallest distances
; will assume lenght of list is greater than n
;(define (n-smallest n lst acc)
;  (if (equal? (length acc) n)
;      acc
;      (let ([least (argmin cdr lst)]) (n-smallest n (remove least lst) (cons least acc)))))


; returns one complete circuit and the remaining edges in edges like
; '(circuit '(edges))
;(define (take-circuit edges)
;  (define (split-pair s)
;    (string-split s "-"))
;  (define (in-circuit? pr circ)
;    (ormap values (map (lambda (x) (string-contains? pr x)) circ)))
;  (define (recursive-take edg circ not-circ added)
;    (cond
;      [(and (empty? edg) (empty? added)) (list (remove-duplicates circ) not-circ)]
;      [(empty? edg) (recursive-take not-circ circ '() '())]
;      [(in-circuit? (car edg) circ)
;       (recursive-take (cdr edg)
;                       (append (split-pair (car edg)) circ)
;                       not-circ
;                       (cons (car edg) added))]
;      [else (recursive-take (cdr edg) circ (cons (car edg) not-circ) added)]))
;  (recursive-take (cdr edges) (split-pair (car edges)) '() '()))
;
;(define (detect-circuits edges)
;  (define (taker-of-circuits edges circs)
;    (if (empty? edges)
;        circs
;        (let ([res (take-circuit edges)]) (taker-of-circuits (second res) (cons (first res) circs)))))
;  (taker-of-circuits (sort edges string<?) '()))
