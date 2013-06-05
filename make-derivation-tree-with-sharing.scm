;;; Coursework 2
;;; Question 5
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


;;; make-derivation-tree-with-sharing

(define make-derivation-tree-with-sharing
  (lambda (e graph)
    (let ((s (successors e graph)))
      (if (null? s)
          (save-cons e '())
          (save-cons e (make-derivation-tree-with-sharing* s graph))))))

(define make-derivation-tree-with-sharing*
  (lambda (s graph)
    (if (null? s)
        '()
        (cons (make-derivation-tree-with-sharing (derivation-dst (car s)) graph)
              (make-derivation-tree-with-sharing* (cdr s) graph)))))

(define save-cons
  (let* ((visited-list '())
         (add-entity (lambda (e)
                       (set! visited-list (cons e visited-list))
                       (car visited-list))))
    (lambda (arg1 arg2)
      (let ((val (cons arg1 arg2)))
        (if (member val visited-list)
            (car (member val visited-list))
            (add-entity val))))))

;;; The function takes an entity identifier and provenance graph and constructs
;;; an n-ary tree in which all nodes are entity identifiers. It also preserves
;;; the sharing of derivation chains in memory.
;;; This is similar to the make-derivation-tree solution except the standard
;;; cons function is replaced with the save-cons function.
;;; save-cons first creates a list to save visited nodes and a function to add
;;; nodes to the list. save-cons takes two arguments, performs the normal
;;; cons function and then checks whether the entity is in the list. If it is,
;;; use that entity rather than creating a new one. Otherwise, save it.