;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Norvig's PAIP chapter 5 translated to R7RS Scheme

;;;;;;;;;;;;;;;;;;;;;;;
;; ELIZA4	     ;;
;; segment variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme load)
	(scheme write))


;;;
;;; PATTERN MATCHING
;;; 

;;; pattern-match : list x list -> [alist | symbol]
(define (pattern-match pattern input)
  ;; obtain the variable bindings (starts with no bindings)
  (bind-matches pattern input '()))

;;; bind-matches : list x list x [alist | symbol] -> [alist | symbol]
(define (bind-matches pattern input bindings)
  ;; match pattern against input in the context of bindings
  ;; returns an extended version of bindings when a new match
  ;; is found
  (cond ((failure?  bindings)  fail)
	((variable? pattern)   (match-variable pattern input bindings))
	((same? pattern input) bindings)
	((segment? pattern)    (match-segment pattern input bindings))
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

;;; segment? : obj -> boolean
(define (segment? x)
  ;; checks if x is a list ((?* y) ...) where y is a variable
  (and (pair? x)
       (pair? (car x))
       (eq? (caar x) '?*)
       (not (null? (cdar x)))
       (variable? (cadar x))))

;;; segment-variable : lst -> symbol
(define (segment-variable lst)
  ;; return ?y in (?* ?y)
  (cadr lst))

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

;;; match-variable : symbol x obj x alist -> [alist | symbol]
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

;;; match-segment : list x obj x alist -> [alist | symbol]
(define (match-segment pattern input bindings)
  (define variable (segment-variable (first pattern)))
  (define binding (bindings-find bindings variable))
  (if (not binding)
      (try-unbounded-segment variable '() (rest pattern) input bindings)
      (try-bounded-segment variable binding (rest pattern) input bindings)))

;;; try-unbounded-segment : symbol x list x list x list x alist -> [alist | symbol]
(define (try-unbounded-segment variable segment pattern input bindings)
  (define binds (try-new-segment variable segment pattern input bindings))
  (if (failure? binds)
      (if (null? input)
	  fail
	  (try-unbounded-segment variable
				 (append segment (list (car input)))
				 pattern
				 (rest input)
				 bindings))
      binds))

;;; try-bounded-segment : symbol x pair x list x list x alist -> [alist | symbol]
(define (try-bounded-segment variable binding pattern input bindings)
  (define segment (binding-value binding))
  (define rest-input (matching-rest input segment))
  (if rest-input
      (bind-matches pattern rest-input bindings)
      fail))

(define (matching-rest lst prefix)
  (cond ((null? prefix) lst)
	((and (not (null? lst)) (equal? (car lst) (car prefix)))
	 (matching-rest (cdr lst) (cdr prefix)))
	(else #false)))

;;; try-new-segment : symbol x list x x list x list x alist -> [alist | symbol]
(define (try-new-segment variable segment pattern input bindings)
  (bind-matches pattern input (bindings-extend bindings variable segment)))

;;; bindings-instantiate : alist x pair -> pair
(define (bindings-instantiate bindings tree)
  (cond ((null? tree) '())
	((pair? tree) (cons (bindings-instantiate bindings (car tree))
			    (bindings-instantiate bindings (cdr tree))))
	((bindings-find bindings tree) => binding-value)
	(else tree)))


;;;;;;;;;;;;;;;;;;;;
;; TESTING ELIZA4 ;;
;;;;;;;;;;;;;;;;;;;;

(load "tester.scm")

(tester
 "No segment variables"

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

(tester
 "With segment variables"

 (test/equal "Single segment variable successful match in the begining"
	     (pattern-match '((?* ?X) is my name) '(Eduardo Acuña Yeomans is my name))
	     '((?X Eduardo Acuña Yeomans)))

 (test/equal "Single segment variable successful match in the end"
	     (pattern-match '(my name is (?* ?X)) '(my name is Eduardo Acuña Yeomans))
	     '((?X Eduardo Acuña Yeomans)))

 (test/equal "Single segment variable successful match in the middle"
	     (pattern-match '(I love (?* ?P) so much) '(I love programming in Scheme so much))
	     '((?P programming in Scheme)))

 (test/equal "One segment and one simple variable successful match"
	     (pattern-match '((?* ?P) need a ?X) '(Mr Hulot and I need a vacation))
	     '((?X . vacation) (?P Mr Hulot and I)))

 (test/equal "Multiple segment variables successful matches"
	     (pattern-match '((?* ?P) need (?* ?X)) '(Mr Hulot and I need a vacation))
	     '((?X a vacation) (?P Mr Hulot and I)))

 (test/equal "Single segment fail successful match"
	     (pattern-match '((?* ?X) is a (?* ?Y)) '(what he is is a fool))
	     '((?Y fool) (?X what he is)))

 (test/equal "Multiple segment fails with repeated segment successful match"
	     (pattern-match '((?* ?X) a b (?* ?X)) '(1 2 a b a b 1 2 a b))
	     '((?X 1 2 a b))))
