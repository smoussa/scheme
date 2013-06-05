;;; Coursework 2
;;; Question 4
;;; Samir Moussa, sm28g11@soton.ac.uk

;;; solution:

(define statement-id cadr)
(define derivation-src caddr)
(define derivation-dst cadddr)

(define successors
  (lambda (e graph)
    (cond ((null? graph) '())
          ((and (equal? 'wasDerivedFrom (caar graph))
                (equal? e (derivation-src (car graph))))
           (cons (car graph) (successors e (cdr graph))))
          (else (successors e (cdr graph))))))

;;; spanning-tree

(define tree-label car)
(define tree-children cdr)

(define spanning-tree
  (let ((entity-list '()))
    (lambda (tree)
      (let ((label (tree-label tree)))
        (cond ((null? tree) '())
              ((memq label entity-list) '())
              (else (set! entity-list (cons label entity-list))
                    (cons label
                          (spanning-tree* (tree-children tree)))))))))

(define spanning-tree*
  (lambda (tree)
    (if (null? tree)
        '()
        (remove-double-list (cons (spanning-tree (tree-label tree))
                                  (spanning-tree* (tree-children tree)))))))

(define remove-double-list
  (lambda (tree)
    (if (and (pair? tree)
             (null? (car tree)))
        (cdr tree)
        tree)))

;;; The function spanning-tree takes an n-ary tree representing an
;;; acyclic provenance graph and returns a spanning tree for that
;;; graph.
;;; spanning-tree first creates an empty 'global' list to store all
;;; visited entities. It traverses through the tree until it has
;;; visited all reachable nodes.The list is accessible only within
;;; the scope of the closure.
;;; spanning-tree* deals with the list of trees.
;;; remove-double-list takes a tree and returns a single empty list
;;; if an empty list within an empty list is come across.
