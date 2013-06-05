;;; Coursework 1
;;; Question 3
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:


(define symmetric
  (lambda (r)
    (if (null? r)
        #t
        (find-pair r r))))

(define find-pair
  (lambda (r1 r2)
    (cond ((null? r1) #t)
          ((member (cons (cdar r1) (caar r1)) r2)
           (find-pair (cdr r1) r2))
          (else #f))))


;;; The predicate symmetric takes a relation r and checks if the relation is
;;; a symmetric binary relation. If the relation is not null, it passes
;;; the relation to a recursive function, find-pair, that takes two argyments
;;; - two of the same list. It recursively checks if each member in the
;;; list has a pair in the same list.
;;; The function find-pair will return true if each element has a symmetric
;;; partner and the end of the list is reached. Otherwise, if a pair has no symmetric
;;; partner the function returns false for the entire relation.