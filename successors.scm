;;; Coursework 2
;;; Question 1
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:

(define statement-id cadr)
(define derivation-src caddr)
(define derivation-dst cadddr)

;;; successors

(define successors
  (lambda (e graph)
    (cond ((null? graph) '())
          ((and (equal? 'wasDerivedFrom (caar graph))
                (equal? e (derivation-src (car graph))))
           (cons (car graph) (successors e (cdr graph))))
          (else (successors e (cdr graph))))))

;;; The function successors takes an entity identifier, graph and returns
;;; a list of derivation statements that have this identity identifier as
;;; a source. The graph is traversed recursively from that point and
;;; returns the list of derivation statements for that entity. If none
;;; are found, an empty list is returned.