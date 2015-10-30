;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ELIZA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings.)"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  "does pattern match input? any variable can match anything"
  (if (variable-p pattern)
	t
	(if (or (atom pattern ) (atom input))
	  (eql pattern input)
	  (and (pat-match (first pattern) (first input))
		   (pat-match (rest pattern) (rest input))))))

(pat-match ´(?X is ?X) ´(2 is 4))