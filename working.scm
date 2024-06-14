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

#|
Ok lets do this


                                                   (cc 11 2)
                                                  /
                                         (cc 11 3)
                                        /         \
                               (cc 11 4)           (cc 1 3)
                              /         \                  \
(count-change 11) -> (cc 11 5)           (cc -14 4) -> 0    (cc -9 3) -> 0
                              \
                               (cc -49 5) -> 0
|#
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
; }}}
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
; Exercise 1.12 {{{
(define (pascal x y)
  (cond ((= x 1) 1)
	((= x y) 1)
	((< x 1) 0)
	((< y 1) 0)
	((> x y) 0)
	(else (+ (pascal x (- y 1)) (pascal (- x 1) (- y 1))))))
; }}}
; Exercise 1.13 {{{
#|
This one is a woozy
phi = (1 + (sqrt 5))/2
psi = (1 - (sqrt 5))/2
We know for both psi and phi as x hold true
(square x) = x + 1
Assume the relation
Fib(n) = (phi^n - psi^n)/(sqrt 5)
Verify base cases
Fib(0) = 0
Fib(1) = 1

Assume 
Fib(k) = (phi^k - psi^k)/(sqrt 5)
Fib(k+1) = (phi^(k+1) - psi^(k+1))/(sqrt 5)

Fib(k+2) = Fib(k) + Fib(k+1)
expanding we get
Fib(k+2) = (phi^k + phi^(k+1) - psi^k - psi^(k-1))/(sqrt 5)
which can be factorised down to
Fib(k+2) = (phi^k * (1 + phi) - psi^k * (1 + psi))/(sqrt 5)
we know that 1+phi = phi squared (same for psi) so
Fib(k+2) = (phi^(k+2) - psi^(k+2))/(sqrt 5)
so by induction the relation hold for all n > 0


psi/(sqrt 5) is approx. 0
so Fib(n) is approx. phi^n/(sqrt 5)
so Fib(n) is the closest integer

Fuck dude I am not ready for Chapter 4

|#
; }}}
; }}}
; }}}
