;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Norvig's PAIP chapter 5 translated to R7RS Scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELIZA2		    ;;
;; returning matched values ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; substitute-list : alist x tree -> tree
(define (substitute-list alist tree)
  ;; Make a tree with the value substitutions described by alist
  (cond ((null? tree) '())
	((pair? tree) (cons (substitute-list alist (car tree))
			    (substitute-list alist (cdr tree))))
	((assoc tree alist) => cdr)
	(else tree)))

;;; fail -> (quote fail)
(define fail
  ;; Indicates pattern-match failure
  'fail)

;;; failute? : obj -> boolean
(define (failure? x)
  (eq? x 'fail))

;;; get-binding : symbol x alist -> [pair | #f]
(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list
  (assq var bindings))

;;; binding-val : pair -> obj
(define (binding-val binding)
  ;; Get the value part of a single binding
  (cdr binding))

;;; lookup : symbol x alist -> [obj | error]
(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list
  (binding-val (get-binding var bindings)))

;;; extend-bindings : symbol x obj x alist -> alist
(define (extend-bindings var val bindings)
  ;; Add a (variable . value) pair to a binding list
  (cons (cons var val) bindings))

;;; pattern-match* : list x list x [alist | fail] -> [alist | fail]
(define (pattern-match* pattern input bindings)
  ;; Match pattern against input in the context of the bindings
  (cond ((failure? bindings)
	 fail)
	((variable? pattern)
	 (match-variable pattern input bindings))
	((eqv? pattern input)
	 bindings)
	((and (pair? pattern) (pair? input))
	 (pattern-match* (cdr pattern)
			 (cdr input)
			 (pattern-match* (car pattern)
					 (car input)
					 bindings)))
	(else fail)))

;;; match-variable : symbol x obj x alist -> [alist | fail]
(define (match-variable var input bindings)
  ;; Does var match input? Uses or updates and returns bindings
  (define binding (get-binding var bindings))
  (cond ((not binding) (extend-bindings var input bindings))
	((equal? input (binding-val binding)) bindings)
	(else fail)))

;;; pattern-match : list x list -> alist
(define (pattern-match pattern input)
  (pattern-match* pattern input '()))

;;;;;;;;;;;;;;;;;;;;
;; TESTING ELIZA2 ;;
;;;;;;;;;;;;;;;;;;;;

(define (eliza2-test)
  (display "ELIZA2 TEST\n\n")

  (display "(pattern-match '(i need a ?X) '(i need a vacation)) =?= ((?X . vacation))\n")
  (display "]=> ")
  (display (pattern-match '(i need a ?X) '(i need a vacation)))

  (newline)
  (newline)

  (display "(substitute-list (pattern-match ('i need a ?X) '(i need a vacation))\n")
  (display "                 '(what would it mean to you if you got a ?X ?))\n")
  (display "=?= (what would it mean to you if you get a vacation ?)\n")
  (display "]=> ")
  (display (substitute-list (pattern-match '(i need a ?X) '(i need a vacation))
			    '(what would it mean to you if you got a ?X ?)))

  (newline)
  (newline)

  (display "(pattern-match '(i need a ?X) '(i really need a vacation)) =?= fail\n")
  (display "]=> ")
  (display (pattern-match '(i need a ?X) '(i really need a vacation)))

  (newline)
  (newline)

  (display "(pattern-match '(this is easy) '(this is easy)) =?= ()\n")
  (display "]=> ")
  (display (pattern-match '(this is easy) '(this is easy)))

  (newline)
  (newline)

  (display "(pattern-match '(?X is ?X) '((+ 2 2) is 4)) =?= fail\n")
  (display "]=> ")
  (display (pattern-match '(?X is ?X) '((+ 2 2) is 4)))

  (newline)
  (newline)

  (display "(pattern-match '(?X is ?X) '((+ 2 2) is (+ 2 2))) =?= ((?X + 2 2))\n")
  (display "]=> ")
  (display (pattern-match '(?X is ?X) '((+ 2 2) is (+ 2 2))))

  (newline)
  (newline)

  (display "(pattern-match '(?P need . ?X) '(i need a long vacation))\n")
  (display "=?= ((?X a long vacation) (?P . i))\n")
  (display (pattern-match '(?P need . ?X) '(i need a long vacation)))

  (newline)
  (newline))

