;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Norvig's PAIP chapter 5 translated to R7RS Scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELIZA3			     ;;
;; more schemefication of the source ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme load))


;;;
;;; PATTERN MATCHING
;;; 

;;; pattern-match : list x list -> alist
(define (pattern-match pattern input)
  ;; obtain the variable bindings (starts with no bindings)
  (bind-matches pattern input '()))

;;; bind-matches : list x list x alist -> alist
(define (bind-matches pattern input bindings)
  ;; match pattern against input in the context of bindings
  ;; returns an extended version of bindings when a new match
  ;; is found
  (cond ((failure?  bindings)  fail)
	((variable? pattern)   (match-variable pattern input bindings))
	((same? pattern input) bindings)
	((and (compound? pattern)
	      (compound? input))
	 (bind-matches (rest pattern)
		       (rest input)
		       (bind-matches (first pattern)
				     (first input)
				     bindings)))
	(else fail)))

;;;
;;; PATTERNS AND INPUTS
;;;

;;; variable? : obj -> boolean
(define (variable? x)
  ;; checks if x is a symbol with a ? prefix
  (and (symbol? x)
       (char=? #\? (string-ref (symbol->string x) 0))))

;;; same? : obj x obj -> boolean
(define (same? x1 x2)
  ;; checks if two non-compound objects are the same
  (eqv? x1 x2))

;;; compound? : obj -> boolean
(define (compound? x)
  ;; checks if x is a compound object
  (pair? x))

;;; first : obj -> obj
(define (first x)
  (car x))

;;; rest : obj -> obj
(define (rest x)
  (cdr x))

;;;
;;; BINDINGS
;;;

(define fail
  ;; fail is bounded to the symbol (quote fail)
  'fail)

;;; failure? : obj -> boolean
(define (failure? x)
  (eq? x 'fail))

;;; bindings-find : alist x symbol -> [pair | boolean]
(define (bindings-find bindings variable)
  (assq variable bindings))

;;; bindings-extend : alist x symbol x obj -> alist
(define (bindings-extend bindings variable input)
  (cons (cons variable input) bindings))

;;; binding-variable : pair -> symbol
(define (binding-variable binding)
  (car binding))

;;; binding-value : pair -> obj
(define (binding-value binding)
  (cdr binding))

;;; match-variable : symbol x obj x alist -> alist
(define (match-variable variable input bindings)
  ;; check if variable is bounded to input in bindings
  ;; extend if variable is not in bindings
  (define binding (bindings-find bindings variable))
  (cond ((not binding)
	 (bindings-extend bindings variable input))
	((equal? input (binding-value binding))
	 bindings)
	(else
	 fail)))

;;; bindings-instantiate : alist x pair -> pair
(define (bindings-instantiate bindings tree)
  (cond ((null? tree) '())
	((pair? tree) (cons (bindings-instantiate bindings (car tree))
			    (bindings-instantiate bindings (cdr tree))))
	((bindings-find bindings tree) => binding-value)
	(else tree)))

;;;;;;;;;;;;;;;;;;;;
;; TESTING ELIZA3 ;;
;;;;;;;;;;;;;;;;;;;;

(load "tester.scm")

(tester
 "Third approximation to the ELIZA program"

 (test/equal "Single simple successful match"
	     (pattern-match '(i need a ?X) '(i need a vacation))
	     '((?X . vacation)))

 (test/equal "Single simple match instantiation"
	     (bindings-instantiate (pattern-match '(i need a ?X) '(i need a vacation))
				   '(what would it mean to you if you got a ?X ?))
	     '(what would it mean to you if you got a vacation ?))

 (test/equal "Single simple failed match"
	     (pattern-match '(i need a ?X) '(i really need a vacation))
	     fail)

 (test/equal "All match no bindings"
	     (pattern-match '(this is easy) '(this is easy))
	     '())

 (test/equal "Single compound failed match"
	     (pattern-match '(?X is ?X) '((+ 2 2) is 4))
	     fail)

 (test/equal "Single compound successful match"
	     (pattern-match '(?X is ?X) '((+ 2 2) is (+ 2 2)))
	     '((?X + 2 2)))

 (test/equal "Multiple compound and rest successful matches"
	     (pattern-match '(?P need . ?X) '(i need a long vacation))
	     '((?X a long vacation) (?P . i))))

