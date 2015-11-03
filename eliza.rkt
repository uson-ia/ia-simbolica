#lang racket

(provide pat-match)
(provide position)

(define fail 'fail)
(define no-bindings '((#t . #t)))

(define (atom? x)
  ;(and (not (null? x))
       (not (pair? x)))

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
        ((segment-pattern? pattern)
         (segment-match pattern input bindings))
        ((and (cons? pattern) (cons? input))
         (pat-match (cdr pattern) (cdr input)
                    (pat-match (car pattern) (car input)
                                bindings)))
        (#t fail)))

(define (segment-match pattern input bindings (start 0))
  (let ((var (second (firts pattern)))
        (pat (rest pattern)))
      (if (null? pat)
          (match-variable var input bindings)
          (let ((pos (position (first pat) input start)))
            (if (null? pos)
                fail
                (let ((b2 (pat-match pat (subseq input pos) bindings)))
                    (if (eqv? b2 fail)
                        (segment-match pattern input bindings (+ pos 1))
                        (match-variable var (subseq input 0 pos) b2))))))))

(define (subseq l offset n)
    (when (and (>= offset 0) (<= n (sub1 length l)))
        (take (drop l offset) n)))

(define (segment-pattern? pattern)
  (and (cons? pattern)
       (starts-with? (car pattern) '?*)))

(define (starts-with? list x)
  (and (cons? list) (eqv? (car list) x)))

(define (match-variable var input bindings)
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal? input (binding-val binding)) bindings)
              (#t fail))))

(define (variable-p x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\?)))

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

;(pat-match '(?X + ?X) '((2 + 2) + (2 + 2)))
;(pat-match '(?P need . ?X) '(i need a long vacation))
;(pat-match '((?* ?x) a b (?*?x)) '(1 2 a b a b 1 2 a b))
;(pat-match '((?* ?p) need '(?* ?)) '(Mr Hulot and I need a vacation))
