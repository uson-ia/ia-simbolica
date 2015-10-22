#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (simple-equal x y)
  (if (or (atom? x) (atom? y))
      (equal? x y )
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(define (pat-match pattern input)
 (if (variable-p pattern)
   ;#t
   (list (cons patter input))
   (if (or (atom? pattern) (atom? input))
       (equal? pattern input)
       (append (pat-match (first pattern) (first input))
            (pat-match (rest pattern) (rest input))))))

; Devuelve True si x es '(?X)
(define (variable-p x)
  (and (list? x)
       (equal? (length x) 1)
       (equal? (string-ref (symbol->string (first x)) 0) #\?)))
