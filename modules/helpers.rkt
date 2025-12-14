(provide csv->nums remove-parens)

(define (csv->nums str [sep ","]) (map string->number (string-split (middle-string str) sep)))

; actually removes first and last character no matter what it is
(define (remove-parens str) (substring str 1 ((compose sub1 string-length) str)))
