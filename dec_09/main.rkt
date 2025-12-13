#lang racket

; "2,5" -> '(2, 5)
(define (c->num c) (map string->number (string-split c ",")))

; (area "2,5" "9,7") gives 24
(define (area corner1 corner2)
  (apply * (map (compose add1 abs) (map - corner1 corner2))))

; part 1
(define (areas tile-list) 
  (define (car-v-cdr ls)
    (define t1 (car ls))
    (map (lambda (t2) (cons (list t1 t2) (area t1 t2))) (cdr ls)))
  (define (check-pairs tls out) 
    (if 
      (< (length tls) 2) out
      (check-pairs (cdr tls) (append (car-v-cdr tls) out))))
  (check-pairs tile-list '()))


; for part 2 we need to check that all points inside the area are within the 
; perimeter defined by the tile list. we can do it by the ray trace parity check
(define (edgelist tl acc) 
  (if (< (length tl) 2) 
    acc
    (edgelist (cdr tl) (cons (take tl 2) acc))))


(define (points-in-rect c1 c2)
  (define xmin (min (first c1) (first c2)))
  (define xmax ((compose add1 max) (first c1) (first c2)))
  (define ymin (min (second c1) (second c2)))
  (define ymax ((compose add1 max) (second c1) (second c2)))
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) (range ymin ymax))) (range xmin xmax))))

; '((  x1   y1) (  x2   y2))
(define (horizontal? edge)
  (define tail (first edge))
  (define head (second edge))
  (equal? (second tail) (second head)))
(define (vertical? edge)
  (define tail (first edge))
  (define head (second edge))
  (equal? (first tail) (first head)))

(define (on-edge? pt edge) 
  (define xmin (apply min (map first edge)))
  (define xmax (apply max (map first edge)))
  (define ymin (apply min (map second edge)))
  (define ymax (apply max (map second edge)))
  (if (horizontal? edge)
    (and (equal? (second pt) ymin) 
         (and (>= (first pt) xmin) (<= (first pt) xmax)))
    (and (equal? (first pt) xmin) 
         (and (>= (second pt) ymin) (<= (second pt) ymax)))))

; I figure I could memoize this if it's slow for big input
(define (red/green? pt edgelist)
  ; how far to cast rays 
  (define xmax (+ 2 (apply max (map (lambda (x) (apply max (map first x))) edgelist))))

  ; shifts a point up by 1/2
  (define (shift-up v)
    (map + v '(0 .5)))

  ; if vertex v is an endpoint in the edge e, shift that endpoint up (y+ dir)
  ; by an infinitesimal amount (1/2 since we're working with integer coordinates)
  (define (adjust-edge v e)
    (define from (first e))
    (define to (second e))
    (define (cond-adj x)
      (if (equal? v x)
          (shift-up x)
          x))
    (list (cond-adj from) (cond-adj to)))

  ; casts a ray from org in the positive x direction
  (define (cast-ray org)
    (map (lambda (x) (map + org (list x 0))) (range (- xmax (first org)))))

  ; checks if a point is on any edge in the list verts
  (define (on-perimeter? pt verts)
    (ormap (lambda (e) (on-edge? pt (adjust-edge pt e))) verts))

  ; walks along ray, counting how many times it crosses an edge
  (define (raytrace ray verticals acc)
    (if (empty? ray)
        acc
        (raytrace (cdr ray) verticals (+ acc (if (on-perimeter? (car ray) verticals) 1 0)))))

  ; does a first pass over all edges. if the point pt passed to this function
  ; is  on an edge, return #t, if not and the edge is vertical, add it to vertical
  ; list (only vertical crossings are relevant) if we get through the whole edge
  ; list do ray trace check on the vertical list (point is inside if number of 
  ; crossings is odd
  (define (checker edgelist verticals)
    (cond
      [(empty? edgelist) (odd? (raytrace (cast-ray pt) verticals 0))] ; does the crossings test
      [(on-edge? pt (car edgelist)) #t]
      [else
       (checker (cdr edgelist)
                (if (vertical? (car edgelist)) (cons (car edgelist) verticals) verticals))]))
  (checker edgelist '()))

(define (r/g/memo? pt edgelist hmap) 
  (cond [(not (hash-has-key? hmap pt)) (hash-set! hmap pt (red/green? pt edgelist))])
  (hash-ref hmap pt))

(define (valid-square? c1 c2 edges) 
  (andmap (lambda (x) (r/g/memo? x edges hmap)) (points-in-rect c1 c2)))

; it's just p1 with a check for valid square, in which case it reports a negative area
(define (areas2 tile-list edges) 
  (define (car-v-cdr ls)
    (define t1 (car ls))
    (map 
      (lambda (t2) 
        (let ([ar (area t1 t2)]) (cons (list t1 t2) (if (valid-square? t1 t2 edges) ar (- 0 ar)))))
      (cdr ls)))
  (define (check-pairs tls out) 
    (if 
      (< (length tls) 2) out
      (check-pairs (cdr tls) (append (car-v-cdr tls) out))))
  (check-pairs tile-list '()))

(define hmap (make-hash))
(define tiles (map c->num (file->lines "input.txt")))
(define edges (edgelist (cons (car tiles) (reverse tiles)) '()))
(define arealist (sort (areas tiles) (lambda (a b) (> (cdr a) (cdr b)))))

(car arealist) ; p1


