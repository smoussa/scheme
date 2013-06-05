;;; Coursework 1
;;; Question 4
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:


(define transpose
  (lambda (m)
    (cond ((null? m) '())
          ((null? (car m)) '())
          (else (cons (map car m) (transpose (map cdr m)))))))


;;; The function transpose takes in a matrix m and tranposes the matrix
;;; by creating a list containing the first elements and concatenating it
;;; with the list of the second elements and the list containing the third
;;; elements, etc. The map function is used to get the element in
;;; position x where x > 0 in each list.