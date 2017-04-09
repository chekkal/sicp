(define (show exp) (call-with-output-string '() (lambda (str)  (display exp str) str)))
(define-macro (assert-equal exp1 exp2) 
	    `(cond 
	     ((not (equal? ,exp1 ,exp2)) (println "Test failed: " (show ,exp1) " not equals to " (show ,exp2)))))

;; (Y f) = (f (Y f)) i.e, for every function applying Y to the function gives us a fixed point of the function.
(define (Y f) ((lambda (x) (f (x x))) (lambda (x) (delay (f (x x))))))

;; define expt using Y combinator
(define (fexp g) (lambda (x n) (cond ((= n 0) 1) (else (* x ((force g) x (- n 1)))))))
(define my-exp (Y fexp))

(let ((x (random-integer 1000)) 
     (n (random-integer 1000)))
  (assert-equal (my-exp x n) (expt x n)))

