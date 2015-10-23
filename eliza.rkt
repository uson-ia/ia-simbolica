#lang racket

(provide pat-match)
(provide atom?)

(define fail null)
(define no-bindings '((#t . #t)))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (simple-equal x y)
  (if (or (atom? x) (atom? y))
      (equal? x y )
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(define (pat-match pattern input (bindings no-bindings))
  (cond ((eq? bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eqv? pattern input) bindings)
        ((and (cons? pattern) (cons? input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                                bindings)))
        (#t fail)))

(define (match-variable var input bindings)
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal? input (binding-val binding)) bindings)
              (#t fail))))

; Devuelve True si x es '(?X)
(define (variable-p x)
  (and (list? x)
       (equal? (length x) 1)
       (equal? (string-ref (symbol->string (first x)) 0) #\?)))


; Asocia var con su valor (lo busca en bindings)
(define (get-binding var bindings)
 (assoc var bindings))

; Devuelve el resto de binding
(define (binding-val binding)
 (cdr binding))

; Asocia var con su valor y devuelve 'rest'
(define (lookup var bindings)
 (binding-val (get-binding var bindings)))

; Agrega el par var val a la lista de pares bindings
(define (extend-bindings var val bindings)
 (cons (cons var val)
       (if (eq? bindings no-bindings)
            null
            bindings)))
