; Skipped excercises {{{
; Exercise 1.42
; }}}
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
; Exercise 1.7 {{{
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
; Exercise 1.9 {{{
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
            (+ (foo (- n 1)))
            (* 2 (foo (- n 2)))))
 (* 3 (foo (- n 3))))
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
        ((< x 1) 0)
        ((< y 1) 0)
        ((> x y) 0)
        (else (+ 
                (pascal x (- y 1)) 
                (pascal (- x 1) (- y 1))))))
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
; Exercise 1.15 {{{
(define (cube x) (* x x x))
(define (p x) 
  (begin
    (display "p")
    (newline)
    (- (* 3 x) (* 4 (cube x)))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; }}}
; Section 1.2.4 {{{
(define (expt-recursive b n)
  (if (= n 0)
    1
    (* b (expt-recursive b (- n 1)))))


(define (expt-linear b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else
          (* b (fast-expt b (- n 1))))))
; }}}
; Exercise 1.16{{{
(define (fast-expt-linear b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1)) 
; }}}
; Exercise 1.17 {{{
(define (fast-mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))
; }}}
; Exercise 1.18 {{{
(define (fast-mult-linear a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (iter a b n)
    (cond ((= b 0) n)
          ((even? b) (iter (double a) (halve b) n))
          (else (iter a (- b 1) (+ n a)))))
  (iter a b 0))
; }}}
; Exercise 1.19 {{{
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ;compute p'
                   (+ (square q) (* 2 p q))  ;compute q'
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))
(fib 30)
; }}}
; Section 1.2.5 {{{
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; }}}
; Exercise 1.20 {{{
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; Remainder has to be calculated for equality check
; (gcd 40 6)
; (gcd 6 (remainder 40 6))
; (gcd 6 4)
; (gcd 4 (remainder 6 4))
; (gcd 4 2)
; (gcd 2 (remainder 4 2))
; (gcd 2 0)
; 2
; Since remainder is calculated imediately every time; it leads to the same number of remainder calls in applicative vs normal order code
; }}}
; Section 1.2.6 {{{
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
; }}}
; Exercise 1.21 {{{
(smallest-divisor 199); => 199
(smallest-divisor 1999); => 1999
(smallest-divisor 19999); => 7
; }}}
; Exercise 1.22 {{{
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (define (iter a)
    (if (= a to)
      '()
      (begin
        (timed-prime-test a)
        (iter (+ a 1)))))
  (iter from))
   
(search-for-primes 1000 1100) ; 3 smallest primes 1009, 1013, 1019
(search-for-primes 10000 10100) ; 3 smallest primes 10007, 10009, 10037
(search-for-primes 100000 100100) ; 3 smallest primes 100003, 100019, 100043
(search-for-primes 1000000 1000100) ; 3 smallest primes 100003, 100019, 100043
; }}}
; Exercise 1.23 {{{
; My computer too fast for this :sob:
; }}}
; The rest of these exercise dont seem worth the time investment
; }}}
; Section 1.3 {{{
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
; Exercise 1.29 {{{
; This was boring so I skipped it
; }}}
; Exercise 1.30 {{{
(define (sum-iter term a next b)
  (define (helper a result)
     (if (> a b)
       result
       (helper (next a)  (+ (term a) result))))
  (helper a 0))

; }}}
; Exercise 1.31 {{{
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact-product n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (product-iter term a next b)
  (define (helper a result)
     (if (> a b)
       result
       (helper (next a)  (* (term a) result))))
  (helper a 1))

(define (identity x) x)
; The formula in the book for Walis' product is confusing so I'm using the wikipedia definition of the thing here
(define (walis-product n)
  (product-iter (lambda (x) (/ (* 4 (square x)) (- (* 4 (square x)) 1))) 1 (lambda (x) (+ x 1)) n))
(* 2.0 (walis-product 1000)) ; it's pretty damn close to pi
; }}}
; Exercise 1.32 {{{
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))
         
(define (accumulate-iter combiner null-value term a next b)
  (define (helper a result)
    (if (> a b)
        result
        (helper (next a) (combiner result a))))
  (iter a null-value))

(define (sum-accum term a next b)
  (accumulate + 0 term a next b))
(define (product-accum term a next b)
  (accumulate * 1 term a next b))
; }}}
; Exercise 1.33 {{{
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (helper a result)
    (if (> a b)
        result
        (helper (next a) (if (predicate a) (combiner result (term a)) result))))
  (iter a null-value))
; These other functions seem simple enough; skipping for now
; }}}
; Exercise 1.34 {{{
; Let's take a look at this seriously:
; (define (f g) (g 2))
; if we call (f f) this is what happens
; (f f)
; (f 2)
; (2 2) => ERROR 2 is not a function
;}}}
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
; Exercise 1.35 {{{
; x -> 1 + 1/x
; x -> x/x + 1/x
; x -> (x + 1)/x
; (multiply both sides by x)
; x^2 -> x + 1 
; which we know is true for phi
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; => 1.6180327868852458
; }}}
; Exercise 1.36 {{{
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2) ; 33
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2) ; 9
;}}}
; Exercise 1.37 {{{
(define (cont-frac n d k)
  (define (helper x)
    (if (>= x k)
      (/ (n k) (d k))
      (/ (n x) (+ (d x) (helper (+ x 1))))))
  (helper 1)) 
(define (cont-frac-iter n d k)
  (define (helper x result)
    (if (>= 0 x)
      result
      (helper (- x 1) (/ (n x) (+ (d x) result)))))
  (helper (- k 1) (/ (n k) (d k)))) 

(/ 1 (cont-frac (lambda (x) 1.0)
                (lambda (x) 1.0)
                11))
 
(/ 1 (cont-frac-iter (lambda (x) 1.0)
                (lambda (x) 1.0)
                11))
; }}}
; Exercise 1.38 {{{
(define (foo x) ((lambda (x) (cond
                               ((= x 1) 1)
                               ((= x 2) 2)
                               ((= (remainder (- x 2) 3) 0) (* (+  1 (/ (- x 2) 3)) 2))
                               (else 1)) 
                  x))) 
(+ (cont-frac-iter (lambda (x) 1.0)
              (lambda (x) foo x)
              50) 2)
; }}}
; Exercise 1.39 {{{
(define (tan-cf y k)
  (cont-frac-iter (lambda (x) (if (= x 1)
                                  y
                                  (square y)))
                  (lambda (x) (- (* 2 x) 1))
                  k))
; }}}
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))
; Exercise 1.40 {{{
(define (cubic a b c)
  (define (cube x) (* x x x))
  (lambda (x) (+ (* a (cube x)) (* b (square x)) c)))
; }}}
; Exercise 1.41 {{{
(define (double g)
  (lambda (x) (g (g x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ; 21
; }}}
; Exercise 1.40 {{{
(define (compose f g)
  (lambda (x) (f (g x))))
; }}}
; Exercise 1.42 {{{
(define (repeated f n)
  (define (helper a result) (if (<= a 0)
                                result
                                (helper (- a 1) (compose f result))))
  (helper (- n 1) f)) 

((repeated inc 4) 5)
                              
     
; }}}
; Exercise 1.42 {{{
(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))     

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
; }}}
; Exercise 1.42 {{{
; No idea about this one
(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) 
    (average x (f x))))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (nth-root x n k)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1)))) (repeated average-damp k) 1.0))

; }}}
; Exercise 1.43 {{{
(define (iterative-improve good-enough? improve)
  (define (helper guess) (if (good-enough? guess)
                             guess
                             (helper (improve guess))))
  (lambda (guess) (helper guess)))
                      
; }}}
; }}}
