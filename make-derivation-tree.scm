;;; Coursework 2
;;; Question 3
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

(define tree-label car)
(define tree-children cdr)

;;; make-derivation-tree

(define make-derivation-tree
  (lambda (e graph)
    (let ((s (successors e graph)))
      (if (null? s)
          (list e)
          (cons e (make-derivation-tree* s graph))))))

(define make-derivation-tree*
  (lambda (s graph)
    (if (null? s)
        '()
        (cons (make-derivation-tree (derivation-dst (car s)) graph)
              (make-derivation-tree* (cdr s) graph)))))

;;; The function make-derivation-tree takes an entity identifier and
;;; a provenance graph and returns an n-ary tree, in which all node
;;; labels are entity identifiers.
;;; make-derivation-tree checks whether a node has successors, if not
;;; the node identifier is returned. Otherwise, the id is added to
;;; the front of the list of successor entities.
;;; make-derivation-tree* deals with the rest of the derivations.
