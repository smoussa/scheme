;;; Coursework 1
;;; Question 2
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:


(define associate
  (lambda (l v)
    (if (null? l)
        l
        (append (list (cons (car l) (vector-ref v (- (vector-length v) (length l)))))
                (associate (cdr l) v)))))


;;; The function associate takes as arguments a list l and a vector v
;;; and returns an association list formed of pairs whose car is an
;;; element of l and cdr the corresponding element of v at the same position.