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
; perimeter defined by the tile list. it requires a lot of resources to do this 
; by the parity check
(define (edgelist tl acc) 
  (if (< (length tl) 2) 
    acc
    (edgelist (cdr tl) (cons (take tl 2) acc))))


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


; an edge is outside a square if both x coordinates or both y coordinates are
; on the same side of the square
(define (edge-in-square? e c1 c2)
  (define (both<= x1 x2 edg)
    (and (<= x1 edg) (<= x2 edg)))
  (define (both>= x1 x2 edg)
    (and (>= x1 edg) (>= x2 edg)))
  (define xmin (min (first c1) (first c2)))
  (define xmax (max (first c1) (first c2)))
  (define ymin (min (second c1) (second c2)))
  (define ymax (max (second c1) (second c2)))
  (not (or (both<= (first (first e)) (first (second e)) xmin)
           (both>= (first (first e)) (first (second e)) xmax)
           (both<= (second (first e)) (second (second e)) ymin)
           (both>= (second (first e)) (second (second e)) ymax))))


(define (square-valid? c1 c2 edges) 
  (define (get-inner-point c1 c2) 
    (define xmin (min (first c1) (first c2)))
    (define ymin (min (second c1) (second c2)))
    (list (add1 xmin) (add1 ymin)))
  (define contains-edge? (ormap (lambda (e) (edge-in-square? e c1 c2)) edges))
  (define (inner-point-valid? c1 c2) (red/green? (get-inner-point c1 c2) edges))
  (and (not contains-edge?) (inner-point-valid? c1 c2)))


(define tiles (map c->num (file->lines "input.txt")))
(define edges (edgelist (cons (car tiles) (reverse tiles)) '()))
(define arealist (sort (areas tiles) (lambda (a b) (> (cdr a) (cdr b)))))

(car arealist) 
(ormap (lambda (x) (if (square-valid? (first (car x)) (second (car x)) edges) x #f)) arealist)



; observation: there is no proposed square entirely outside of the polygon of
; interest since the edges of the polygon contain all possible corners
; if a suqare has a polygon edge in its interior it can't be valid since once 
; side of the edge is inside the poly and the other is outside. on the other 
; hand if there is no polygon edge in the interior the whole interior is either 
; entirely inside or entirely outside the polygon
;
;   invalid no matter what is
;   inside/outside of the 
;   polygon described by ###
;   ......................
;   ...#######............
;   ......ooo#oo..........
;   ......o..##o##........
;   ......o....o..........
;
;   valid no matter what is
;   inside/outside of the 
;   polygon described by ###
;   because the square can't be 
;   on the outside
;   ......................
;   ...#######............
;   ...ooo...#............
;   ...o.o...#####........
;   ...ooo................
; 
;   all interior
;   points must either inside
;   or outside the poly so it's
;   enough to check one point
;   .........oooo.........
;   ...######o..o.........
;   .........#..o.........
;   .........o#o#.........
;   ......................
; 
;   I don't think it is possible 
;   have all interior points in 
;   the square inside the polygon
;   but some points on the edge not 
;   in there. if possible it's 
;   probably quite rare 


