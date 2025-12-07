; interior points on that grid have 8 neighbors, the edges are special
; - if we do the first line alone we can recurse through the rest 
; - if we do the first point on a line alone we could recurse through the rest
; OR we can do it smart: if we add a border of "." around the entire map we only
; have to count interior points, the border will not affect anything

; gets the characters indicated by x in the diagram below:
; t = "xxx....."
; m = "x.x....."
; b = "xxx....."
(define (get-box t m b) 
  (string-join (list (substring t 0 3) (substring m 0 1) (substring m 2 3) (substring b 0 3))))

; counts the @s in a string
(define (count@ str) 
  (foldl + 0 (map (lambda (x) (if (equal? x #\@) 1 0)) (string->list str))))

(define (@? str) (equal? "@" str))

; checks if the char indicated by x is an @ ".x..."
(define (paper? str) (@? (substring str 1 2)))
(define (movable? str) (< (count@ str) 4))
(define (movable-paper? top mid bottom)
  (and (paper? mid) (movable? (get-box top mid bottom))))
(define (end-of-line? str) (< (string-length str) 3))
(define (pop str) (substring str 1))

; builds an all-dots border around the map; technically mirrors the map along 
; the horizontal but that doesn't matter for the solution
(define (addborder mp)
  (define leftright (map (lambda (x) (string-join (list "." x ".") "")) mp))
  (define alldots (make-string (string-length (first leftright)) #\.))
  (cons alldots (reverse (cons alldots leftright))))

(define (checkline top middle bottom ct)
  (if (not (end-of-line? middle))
      (checkline (pop top)
                 (pop middle)
                 (pop bottom)
                 (+ (if (movable-paper? top middle bottom) 1 0) ct))
      ct))

(define (checkmap mp ct)
  (if (< (length mp) 3)
      ct
      (checkmap (cdr mp) (+ ct (checkline (first mp) (second mp) (third mp) 0)))))

(define papermap (addborder (file->lines "input.txt")))
(checkmap papermap 0)

; part 2 is to remove as many moveable papers as possible, which requires an
; updating of the map and presumably several passes over the new maps generated
; the change over checkline is basically to return also an updated row with the
; moveable papers replaced with .
(define (cleanline top mid bottom ct newmid)
  (if (not (end-of-line? mid))
      (cleanline (pop top)
                 (pop mid)
                 (pop bottom)
                 (+ (if (movable-paper? top mid bottom) 1 0) ct)
                 (cons (if (movable-paper? top mid bottom)
                           "x"  ; OK to mark removed paper as x; equivalent to .
                           (substring mid 1 2))
                       newmid))
      (cons (string-join newmid "") ct)))


; does one pass over the map, removes what can be removed, returns a pair of 
; '(newmap).num_removed where num map has the removed rolls marked as x
; mirrors the map across the vertical but this don't matter
(define (cleanmap mp ct newmp)
  (if (< (length mp) 3)
      (cons (addborder newmp) ct)
      (let ([cleaned (cleanline (first mp) (second mp) (third mp) 0 '())])
        (cleanmap (cdr mp) (+ ct (cdr cleaned)) (cons (car cleaned) newmp)))))

; cleans map iteratively until no more paper can be removed
(define (clear mp ct) 
  (define cleaned (cleanmap mp 0 '()))
  (if (= 0 (cdr cleaned))
      ct
      (clear (car cleaned) (+ ct (cdr cleaned)))))

(define papermap (addborder (file->lines "input.txt")))
(clear papermap 0)
