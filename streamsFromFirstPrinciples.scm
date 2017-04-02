;; Stream from first principles from SICP (Lecture 6: Streams)

;; utils
(define (-1+ n) (- n 1))
(define (1+ n) (+ n 1))
(define (show exp) (call-with-output-string '() (lambda (str)  (display exp str) str)))
(define-macro (assert-equal exp1 exp2) 
	      `(cond 
		 ((not (equal? ,exp1 ,exp2)) (println "Test failed: " (show ,exp1) " not equals to " (show ,exp2)))))

;; Stream Constructor/Selectors
;; delay and force are already defined, but re-defining them is enlightening
(define-macro (delay exp) 
	      `(lambda () ,exp))
(define (force p) 
  (p))
(define-macro (stream h t) 
  `(cons ,h (delay ,t)))
(define (empty-stream? s) 
  (null? s))
(define (make-empty-stream) 
  '())
(define (head s) 
  (car s))
(define (tail s) 
  (force (cdr s)))

;; stream processing functions
(define (map f s)
  (cond
    ((empty-stream? s) (make-empty-stream))
    (else (stream (f (head s)) (map f (tail s))))))
(define (filter pred s) 
  (cond
    ((empty-stream? s) (make-empty-stream))
    ((pred (head s)) (stream (head s) (filter pred (tail s))))
    (else (filter pred (tail s)))))
(define (take n s) 
  (cond 
    ((zero? n) (make-empty-stream)) 
    ((empty-stream? s) (make-empty-stream)) 
    (else (stream (head s) (take (-1+ n) (tail s))))))
(define (to-list s) 
  (cond 
    ((empty-stream? s) (make-empty-stream)) 
    (else (cons (head s) (to-list (tail s))))))

;; Some uses
(define (integers-from n) 
  (stream n (integers-from (1+ n))))

;; stream of all natural numbers
(define integers (integers-from 1))

(assert-equal (to-list (take 21 integers)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))

;; stream of squares of natural numbers 
(define squares (map (lambda (x) (* x x)) integers))

(assert-equal (to-list (take 21 squares)) '(1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361 400 441))

;; Sieve of Eratosthenes
(define (sieve s) 
  (let ((h (head s)))
    (stream 
      h
      (sieve (filter (lambda (e) (not (zero? (remainder e h)))) (tail s))))))
;; stream of all primes
(define primes (sieve (integers-from 2)))
(assert-equal (to-list (take 21 primes)) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73))
