#lang racket

(provide pat-match)

(define fail 'fail)
(define no-bindings '((#t . #t)))

(define (atom? x)
  ;(and (not (null? x))
       (not (pair? x)))

(define (pat-match pattern input (bindings no-bindings))
  (cond ((eq? bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((equal? pattern input) bindings)
        ((segment-pattern? pattern)
         (segment-match pattern input bindings))
        ((and (cons? pattern) (cons? input))
         (pat-match (cdr pattern) (cdr input)
                    (pat-match (car pattern) (car input)
                                bindings)))
        (#t fail)))

(define (segment-match pattern input bindings (start 0))
  (let ((var (second (car pattern)))
        (pat (cdr pattern)))
      (if (null? pat)
          (match-variable var input bindings)
          (let ((pos (position (car pat) input start)))
            (if (null? pos)
                fail
                (let ((b2 (pat-match
                            pat (subseq input pos)
                            (match-variable var (subseq input 0 pos)
                                            bindings))))
                    (if (eq? b2 fail)
                        (segment-match pattern input bindings (+ pos 1))
                        b2)))))))

(define (subseq l offset (n (length l)))
  (if (and (< offset n) (>= offset 0) (< offset (length l)) (<= n (length l)) (>= n 0))
      (if (equal? n (length l))
          (drop l offset)
          (drop (take l n) offset))
      #f))

(define (position item list start)
    (cond
        ((or (> start (length list)) (null? (drop list start))) #f)
        ( (eqv? (car (drop list start)) item) start)
          (else (position item list (add1 start) ) )))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    RUNNING BOOK EXAMPLES    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "--------  Running examples: --------")

(displayln "(pat-match '(?X + ?X) '((2 + 2) + (2 + 2)))")
(display "Result >> ")
(pat-match '(?X + ?X) '((2 + 2) + (2 + 2)))
(displayln "")

(displayln "(?P need . ?X) '(i need a long vacation)")
(display "Result >> ")
(pat-match '(?P need . ?X) '(i need a long vacation))
(displayln "")

(displayln "((?* ?p) need (?* ?x)) '(Mr Hulot and I need a vacation)")
(display "Result >> ")
(pat-match '((?* ?p) need (?* ?x)) '(Mr Hulot and I need a vacation))
(displayln "")

(displayln "((?* ?x) is (?* ?y)) '(what he is is a fool)")
(display "Result >> ")
(pat-match '((?* ?x) is (?* ?y)) '(what he is is a fool))
(displayln "")


(displayln "((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b)")
(display "Result >> ")
(pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
(displayln "")
