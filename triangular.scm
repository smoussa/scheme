;;; Coursework 1
;;; Question 1
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:


(define triangular
  (lambda (n)
    (cond ((< n 0) (error "Argument must be at least 0"))
          ((= n 0) 0)
          (else (+ n (triangular (- n 1)))))))


;;; The function triangular takes as argument a number n and returns
;;; the nth triangular number.
;;; A non-recursive definition would be using the formula 0.5*(n)*(n-1) to
;;; calculate the nth triangular number, which is the sum of the
;;; numbers up to and including n.