;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 Eduardo Acuña Yeomans
;;;
;;; Norvig's PAIP chapter 5 translated to R7RS Scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELIZA5	         ;;
;; Rule-based translator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme load)
	(scheme write)
	(scheme read)
	(scheme cxr)
	(srfi 27))


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

(define (rule-pattern rule)
  (car rule))

(define (rule-responses rule)
  (cdr rule))

(define *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really -- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(define (eliza)
  (let loop ((i 0))
    (display "\neliza[")
    (display i)
    (display "] > ")
    (display (flatten (use-eliza-rules (read))))
    (loop (+ i 1))))

(define (substitute-list alist tree)
  ;; Make a tree with the value substitutions described by alist
  (cond ((null? tree) '())
	((pair? tree) (cons (substitute-list alist (car tree))
			    (substitute-list alist (cdr tree))))
	((assoc tree alist) => cdr)
	(else tree)))

(define (some pred lst)
  (cond ((null? lst) '(no rules available))
	((pred (car lst)) => (lambda (x) x))
	(else (some pred (cdr lst)))))

(define (use-eliza-rules input)
  (some (lambda (rule)
	  (let ((result (pattern-match (rule-pattern rule) input)))
	    (if (not (failure? result))
		(substitute-list (switch-viewpoint result)
				 (random-elt (rule-responses rule)))
		#f)))
	*eliza-rules*))

(define (switch-viewpoint words)
  (substitute-list '((I . you) (you . I) (me . you) (am . are))
		   words))

(define (flatten lst)
  (apply append (map mklist lst)))

(define (mklist x)
  (if (list? x) x (list x)))

(define (random-elt choices)
  (list-ref choices (random-integer (length choices))))

