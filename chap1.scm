
;; Main idea is to build up compund procedures from simple blocks

;; The local name "x" plays the same role that a pronoun plays in natural language
;; To square something, multiply it by itself.
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (+ a 2)))


(define (test-cond-undefined a)
  (cond
    ((= a 1) 1)
    ((= a 2) 5)))

(define (abs x)
  (if (< x 0) (- x) x))


;; Exercise 1.1

10 ;; 10

(+ 5 3 4) ;; 12

(- 9 1) ;; 8

(/ 6 2) ;; 3

(+ (* 2 4) (- 4 6)) ;; 6

(define a 3)
(define b (+ a 1))

(+ a b (* a b)) ;; 19

(= a b) ;; #f

(if (and (> b a) (< b (* a b)))
    a
    b) ;; 3

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16


(+ 2 (if (> b a) b a)) ;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16


;; Exercise 1.2

(/ (+ 5
      (/ 1 2)
      (- 2 3 (+ 6 (/ 1 5))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3

(define (sos-of-larger-two x y z)
  (cond
    ((and (< x y) (< x z)) (sum-of-squares y z))
    ((< y z) (sum-of-squares x z))
    (else (sum-of-squares x y))))

;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; (a-plus-abs-b 3 4)
;; => ((if (> 4 0) + -) 3 4)
;; => ((if #t + -) 3 4)
;; => (+ 3 4)
;; => 7

;; (a-plus-abs-b 3 -2)
;; => ((if (> -2 0) + -) 3 -2)
;; => ((if #f + -) 3 -2)
;; => (- 3 -2)
;; => 5


;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Say Ben Bitdiddle evaluates this expression: (test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
;; Infinite loop. The arguments get evaluated first, so (p) will cause unending loop.

;; What behavior will he observe with an interpreter that uses normal-order evaluation?
;; 0 will be printed. The arguments are not expanded, instead the procedure is expanded.
;; This produces the if statement, which will first evaluate the condition/predicate.
;; This is true, so the consequent is evaluated and returned.




;; Newton's square root method
;; Notice how procedures are broken up into smaller and simpler blocks wherever possible

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

