#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (simple-equal x y)
  (if (or (atom? x) (atom? y))
      (equal? x y )
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))
