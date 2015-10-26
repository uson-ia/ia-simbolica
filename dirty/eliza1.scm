;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Norvig's PAIP chapter 5 translated to R7RS Scheme

;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELIZA1              ;;
;; first approximation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme write))

;;; variable? : obj -> boolean
(define (variable? x)
  ;; Is x a variable (symbol begining with '?')?
  (and (symbol? x)
       (char=? #\? (string-ref (symbol->string x) 0))))

;;; atom? : obj -> boolean
(define (atom? x)
  ;; Is x an atom?
  (not (pair? x)))

;;; pattern-matches? : list x list -> boolean
(define (pattern-matches? pattern input)
  ;; Does the pattern matches the input?
  (cond ((variable? pattern)
	 #true)
	((or (atom? pattern) (atom? input))
	 (eqv? pattern input))
	(else
	 (and (pattern-matches? (car pattern) (car input))
	      (pattern-matches? (cdr pattern) (cdr input))))))

;;;;;;;;;;;;;;;;;;;;
;; TESTING ELIZA1 ;;
;;;;;;;;;;;;;;;;;;;;

(define (eliza1-test)
  (display "ELIZA1 TEST\n\n")
  
  (display "(pattern-matches? '(I need a ?X) '(I need a vacation)) =?= #t\n")
  (display "]=> ")
  (display (pattern-matches? '(I need a ?X) '(I need a vacation)))

  (newline)

  (display "(pattern-matches? '(I need a ?X) '(I really need a vacation)) =?= #f\n")
  (display "]=> ")
  (display (pattern-matches? '(I need a ?X) '(I really need a vacation)))

  (newline)
  (newline))
