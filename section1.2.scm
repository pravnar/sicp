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

;; (+r 4 5)
;; => (inc (+r (dec 4) 5))
;; => (inc (+r 3 5))
;; => (inc (inc (+r (dec 3) 5)))
;; => (inc (inc (+r 2 5)))
;; => (inc (inc (inc (+r (dec 2) 5))))
;; => (inc (inc (inc (+r 1 5))))
;; => (inc (inc (inc (inc (+r (dec 1) 5)))))
;; => (inc (inc (inc (inc (+r 0 5)))))
;; => (inc (inc (inc (inc 5))))
;; => (inc (inc (inc 6)))
;; => (inc (inc 7))
;; => (inc 8)
;; => 9

;; (+i 4 5)
;; => (+i (dec 4) (inc 5))
;; => (+i 3 6)
;; => (+i (dec 3) (inc 6))
;; => (+i 2 7)
;; => (+i (dec 2) (inc 7))
;; => (+i 1 8)
;; => (+i (dec 1) (inc 8))
;; => (+i 0 9)
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

