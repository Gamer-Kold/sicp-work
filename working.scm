; Chapter 1 {{{
; Section 1 {{{
(define (square x) (* x x))
#| Exercise 1.1{{{
Exercise 1.1: Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a) 
         ((< a b) b)
         (else -1)) 
   (+ a 1)) ; 16
}}} |#
; Yeah the rest of these are just as trivial
; MOVING ON TO THE NEXT SECTION
; Section 1.1.7 {{{
(define (sqrt-iter guess x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (define (avg x y) (/ (+ x y) 2))
    (avg guess (/ x guess)))
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
; Excercise 1.7 {{{
(define (better-sqrt-iter prev-guess guess x)
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess)) 0.0001))
  (define (improve guess x)
    (define (avg x y) (/ (+ x y) 2))
    (avg guess (/ x guess)))
  (if (good-enough? guess prev-guess)
    guess
    (better-sqrt-iter guess (improve guess x) x)))
(define (better-sqrt x)
  (better-sqrt-iter 0 1.0 x))
; }}}
; Exercise 1.8 {{{
(define (cube-root x)
  (define (cube-root-iter prev guess x)
    (define (good-enough prev guess)
      (< (abs (- prev guess)) 0.000001))
    (define (improve guess x)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (if (good-enough prev guess)
      guess
      (cube-root-iter guess (improve guess x) x)))
  (cube-root-iter 0 1.0 x))
; }}}
; }}}
; }}}
; Section 1.2 {{{
; Excercise 1.9 {{{
; First is recursive
; Second is iterative
; }}}
; Exercise 1.10 {{{
; implementation of ackermans' function for the exercise. (aint no way I do this by hand)
; (A 1 10) = 1024
; (A 2 4) and (A 3 3) = 65536
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
; This was really hard (and fun!)
; (A 0 n) is equivalent to 2n
; (A 1 n) is equivalent to 2^n
; (A 2 n) is equivalent to 2 raised to itself n times
; What an exercise!
; }}}
; Section 1.2.2 {{{
; Kind of a crazy program ngl
; I dont know if I understand it fully
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50))) 
; Exercise 1.11 {{{
(define (foo n)
  (cond ((< n 3) n)
	(else
	  (+ (foo (- n 1))
	     (* 2 (foo (- n 2)))
	     (* 3 (foo (- n 3)))))))
(define (foo-iter n)
  (define (helper count a b c)
    (if (= count (- n 2))
      c
      (helper (+ count 1)
	      b
	      c
	      (+ c (* 2 b) (* 3 a)))))
  (if (< n 3)
    n
    (helper 0 0 1 2)))

; }}}
; }}}
; }}}
; }}}
