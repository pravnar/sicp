;; Exercise 1.9

(define (+rec a b)
  (if (= a 0)
      b
      (inc (+rec (dec a) b))))

(define (+iter a b)
  (if (= a 0)
      b
      (+iter (dec a) (inc b))))

;; Evaluation using the substitution model

;; (+rec 4 5)
;; => (inc (+rec (dec 4) 5))
;; => (inc (+rec 3 5))
;; => (inc (inc (+rec (dec 3) 5)))
;; => (inc (inc (+rec 2 5)))
;; => (inc (inc (inc (+rec (dec 2) 5))))
;; => (inc (inc (inc (+rec 1 5))))
;; => (inc (inc (inc (inc (+rec (dec 1) 5)))))
;; => (inc (inc (inc (inc (+rec 0 5)))))
;; => (inc (inc (inc (inc 5))))
;; => (inc (inc (inc 6)))
;; => (inc (inc 7))
;; => (inc 8)
;; => 9

;; (+iter 4 5)
;; => (+iter (dec 4) (inc 5))
;; => (+iter 3 6)
;; => (+iter (dec 3) (inc 6))
;; => (+iter 2 7)
;; => (+iter (dec 2) (inc 7))
;; => (+iter 1 8)
;; => (+iter (dec 1) (inc 8))
;; => (+iter 0 9)
;; 9


;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;; > (A 1 10)
;; 1024
;; > (A 2 4)
;; 65536
;; > (A 3 3)
;; 65536

(define (f n) (A 0 n))
;; (f n) = 2n

(define (g n) (A 1 n))
;; (g n) = 2^n

(define (h n) (A 2 n))
;; (h n) = 2^(2^(...^2) (with n '2's in total)
;; Better definition: (h n) = 2^(2^n)

;; (A 2 4)
;; => (A 1 (A 2 3))
;; => (A 1 (A 1 (A 2 2)))
;; => (A 1 (A 1 (A 1 (A 2 1))))
;; => (A 1 (A 1 (A 1 2)))
;; => (A 1 (A 1 2^2))
;; => (A 1 2^(2^2))
;; => 2^(2^(2^2))

;; (A 2 3)
;; => (A 1 (A 2 2))
;; => (A 1 (A 1 (A 2 1)))
;; => (A 1 (A 1 2))
;; => (A 1 2^2)
;; => 2^(2^2)

;; (A 2 2)
;; => (A 1 (A 2 1))
;; => (A 1 2)
;; => 2^2


;; Exercise 1.11
;; f(n) = | n                                  if n<3
;;        | f(n - 1) + 2f(n - 2) + 3f(n - 3)   otherwise

;; Recursive
(define (frec n)
  (if (< n 3)
      n
      (+ (frec (- n 1))
         (* 2 (frec (- n 2)))
         (* 3 (frec (- n 3))))))

;; Iterative
(define (fiter n)
  (fiter-helper 2 1 0 n))

(define (fiter-helper a b c count)
  (cond ((< count 0) count)
        ((= count 0) c)
        ((= count 1) b)
        (else (fiter-helper (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
