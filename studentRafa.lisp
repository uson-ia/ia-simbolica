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

;;;; la funcion principal de Student 
;;;; elimina palabras que no tienen contenido
(defun student (words)
  "Solve certain Algebra Word Problems."
  (solve-equations
    (create-list-of-equations
      (translate-to-expression (remove-if #'noise-word-p words)))))

;;;encuentra una regla en student_rule para transformar la entrada
(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
        words *student-rules*
        :rule-if #'rule-pattern :rule-then #'rule-response
        :action #'(lambda (bindings response)
                    (sublis (mapcar #'translate-pair bindings)
                              response)))
      (make-variable words)))
;;;toma una variable  o valor y lo traduce
(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression (binding-val pair))))
;;; toma una expresion y lo separa en uina lista de ecuaciones
(defun create-list-of-equations (exp)
  "Separate out equations embedded in nested parens."
  (cond ((null exp) nil)
        ((atom (first exp)) (list exp))
        (t (append (create-list-of-equations (first exp))
                   (create-list-of-equations (rest exp))))))

;;; crea una variable que representa una lista de de palabras 
(defun make-variable (words)
  "Create a variable name based on the given list of words"
  (first words))

(defun noise-word-p (word)
  "Is this a low-content word which can be safely ignored?"
  (member word '(a an the this number of $)))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations nil)))

(defun solve (equations known)
  "Solve a system of equations by constraint propagation."
  ;;trata de resolver para una ecuacion y sustituir su valor en otro
  ;;si no funciona regresa lo que conoce
  (or (some #'(lambda (equation)
                (let ((x (one-unknown equation)))
                  (when x
                    (let ((answer (solve-arithmetic
                   (isolate equation x))))
                      (solve (subst (exp-rhs answer) (exp-lhs answer)
                                    (remove equation equations))
                             (cons answer known))))))
            equations)
      known))