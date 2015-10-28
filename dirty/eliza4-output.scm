;;; BEGIN "No segment variables" TESTS

;; 1. Single simple successful match
(pattern-match (quote (i need a ?X)) (quote (i need a vacation)))
;; ==> ((?X . vacation))
;; Passed.

;; 2. Single simple match instantiation
(bindings-instantiate (pattern-match (quote (i need a ?X)) (quote (i need a vacation))) (quote (what would it mean to you if you got a ?X ?)))
;; ==> (what would it mean to you if you got a vacation ?)
;; Passed.

;; 3. Single simple failed match
(pattern-match (quote (i need a ?X)) (quote (i really need a vacation)))
;; ==> fail
;; Passed.

;; 4. All match no bindings
(pattern-match (quote (this is easy)) (quote (this is easy)))
;; ==> ()
;; Passed.

;; 5. Single compound failed match
(pattern-match (quote (?X is ?X)) (quote ((+ 2 2) is 4)))
;; ==> fail
;; Passed.

;; 6. Single compound successful match
(pattern-match (quote (?X is ?X)) (quote ((+ 2 2) is (+ 2 2))))
;; ==> ((?X + 2 2))
;; Passed.

;; 7. Multiple compound and rest successful matches
(pattern-match (quote (?P need . ?X)) (quote (i need a long vacation)))
;; ==> ((?X a long vacation) (?P . i))
;; Passed.

;;; END "No segment variables" TESTS: PASSED
;;;     (Total: 7  Passed: 7  Failed: 0)







;;; BEGIN "With segment variables" TESTS

;; 1. Single segment variable successful match in the begining
(pattern-match (quote ((?* ?X) is my name)) (quote (Eduardo Acuña Yeomans is my name)))
;; ==> ((?X Eduardo Acuña Yeomans))
;; Passed.

;; 2. Single segment variable successful match in the end
(pattern-match (quote (my name is (?* ?X))) (quote (my name is Eduardo Acuña Yeomans)))
;; ==> ((?X Eduardo Acuña Yeomans))
;; Passed.

;; 3. Single segment variable successful match in the middle
(pattern-match (quote (I love (?* ?P) so much)) (quote (I love programming in Scheme so much)))
;; ==> ((?P programming in Scheme))
;; Passed.

;; 4. One segment and one simple variable successful match
(pattern-match (quote ((?* ?P) need a ?X)) (quote (Mr Hulot and I need a vacation)))
;; ==> ((?X . vacation) (?P Mr Hulot and I))
;; Passed.

;; 5. Multiple segment variables successful matches
(pattern-match (quote ((?* ?P) need (?* ?X))) (quote (Mr Hulot and I need a vacation)))
;; ==> ((?X a vacation) (?P Mr Hulot and I))
;; Passed.

;; 6. Single segment fail successful match
(pattern-match (quote ((?* ?X) is a (?* ?Y))) (quote (what he is is a fool)))
;; ==> ((?Y fool) (?X what he is))
;; Passed.

;; 7. Multiple segment fails with repeated segment successful match
(pattern-match (quote ((?* ?X) a b (?* ?X))) (quote (1 2 a b a b 1 2 a b)))
;; ==> ((?X 1 2 a b))
;; Passed.

;;; END "With segment variables" TESTS: PASSED
;;;     (Total: 7  Passed: 7  Failed: 0)
