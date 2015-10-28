;;; BEGIN "Third approximation to the ELIZA program" TESTS

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


;;; END "Third approximation to the ELIZA program" TESTS: PASSED
;;;     (Total: 7  Passed: 7  Failed: 0)
