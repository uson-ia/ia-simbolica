;;;; por Rafael pero pirateardo del libro
;;;; tomado libri Paradigms of AI Programming capituolo 7
;;;; programa que resuelve problemitas de algebra




(defstruct (rule (:type list)) pattern response)

(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
  '(((?x* |.|)                  ?x)
    ((?x* |.| ?y*)          (?x ?y))
    ((if ?x* |,| then ?y*)  (?x ?y))
    ((if ?x* then ?y*)      (?x ?y))
    ((if ?x* |,| ?y*)       (?x ?y))
    ((?x* |,| and ?y*)      (?x ?y))
    ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
    ((find ?x*)             (= to-find ?x))
    ((?x* equals ?y*)       (= ?x ?y))
    ((?x* same as ?y*)      (= ?x ?y))
    ((?x* = ?y*)            (= ?x ?y))
    ((?x* is equal to ?y*)  (= ?x ?y))
    ((?x* is ?y*)           (= ?x ?y))
    ((?x* - ?y*)            (- ?x ?y))
    ((?x* minus ?y*)        (- ?x ?y))
    ((difference between ?x* and ?y*)  (- ?y ?x))
    ((difference ?x* and ?y*)          (- ?y ?x))
    ((?x* + ?y*)            (+ ?x ?y))
    ((?x* plus ?y*)         (+ ?x ?y))
    ((sum ?x* and ?y*)      (+ ?x ?y))
    ((product ?x* and ?y*)  (* ?x ?y))
    ((?x* * ?y*)            (* ?x ?y))
    ((?x* times ?y*)        (* ?x ?y))
    ((?x* / ?y*)            (/ ?x ?y))
    ((?x* per ?y*)          (/ ?x ?y))
    ((?x* divided by ?y*)   (/ ?x ?y))
    ((half ?x*)             (/ ?x 2))
    ((one half ?x*)         (/ ?x 2))
    ((twice ?x*)            (* 2 ?x))
    ((square ?x*)           (* ?x ?x))
    ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
    ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
    ((?x* % ?y*)            (* (/ ?x 100) ?y)))))

