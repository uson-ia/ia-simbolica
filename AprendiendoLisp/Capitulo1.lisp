(+ 2 2)

(+ 1 2 3 4 5 6 7 8 9 10)

(- (+ 9000 900 90 9) (+ 5000 500 50 5))

(append '(Pat Kim) '(Robin Sandy))

'(Pat Kim)

'John

'(John Q Public)

'2

2

'(+ 2 2)

(+ 2 2)

John

(John Q Public)

(append '(Pat Kim) (list '(John Q Publ ic) 'Sandy)) 

(length (append '(Pat Kim) (list '(John Q Public) 'Sandy))) 

(setf p '(John Q Public)) 

p

(setf x 10)

(+ x x) 

(+ x (length p))

(first p)

(rest p) 

(second p)

(third p)

(fourth p)

(length p)

(setf x '((1st element) 2 (element 3) ((4)) 5)) 

(length x)

(first X) 

(second x)

(third x) 

(fourth x)

(first (fourth x)) 

(first (first (fourth X)))

(fifth x)

(first X) 

(second (first x)) 

p

(cons 'Mr p) 

(cons (first p) (rest p)) 

(setf town (list 'Anytown 'USA)) 

(list p 'of town 'may 'have 'already 'won!)

(append p '(of) town '(may have already won!)) 

p

(last p) 

(first (last p)) 

(defun last-name (name)
	"Select the last name from a name represented as a list."
	(first (last name))) 

(last-name p)

(last-name '(Rear Admiral Grace Murray Hopper))

(last-name '(Rex Morgan MD)) 

(last-name '(Spot)) 

(last-name '(Aristotle)) 

(defun first-name (name)
	"Select the first name from a name represented as a list."
	(first name)) 

p

(first-name p)

(first-name '(Wilma Flintstone)) 

(setf names '((John Q Public) (Malcolm X)
(Admiral Grace Murray Hopper) (Spot)
(Aristotle) (A A Milne) (Z Z Top)
(Sir Larry Olivier) (Miss Scarlet))) 

(first-name (first names)) 

(mapcar #'last-name names) 

(mapcar #'- '(1 2 3 4))

(mapcar #'+ '(1 2 3 4) '(10 20 30 40))

(mapcar #'first-name names) 

(defparameter *titles*
'(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
"A list of titles that can appear at the start of a name.") 