;;; Coursework 2
;;; Question 2
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

;;; all-derivation-chains

(define all-derivation-chains
  (lambda (e graph)
    (let ((s (successors e graph)))
      (derivation-chain* s graph))))

(define derivation-chain*
  (lambda (s graph)
    (if (null? s)
        '()
        (append (derivation-chain (car s) graph)
                (derivation-chain* (cdr s) graph)))))

(define derivation-chain
  (lambda (d graph)
    (let ((id (statement-id d))
          (dst (derivation-dst d)))
      (if (null? (successors dst graph))
          (list (list id))
          (map (lambda (chain)
                 (cons id chain))
               (all-derivation-chains dst graph))))))

;;; The function all-derivation-chains takes an entity identifier and
;;; a provenance graph and returns all derivation chains starting with
;;; this entity.
;;; derivation-chain computes the derivation chain of a single derivation
;;; by concatenating its statement id to each derivation chain of its
;;; derivation destination entity.
;;; derivation-chain* deals with the list of derivations. The two are
;;; mutually recursive functions.