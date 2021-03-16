; Luke Vu
; CS450
; Homework 2
; 02 Oct 2020

; Question 1
(define (is-list? x)
  (cond ((pair? x)
         (if (equal? (cdr x) '())
             #t
             (is-list? (cdr x))))
        ((null? x) #t)
         (else #f)))


; Question 2
(define (my-reverse mylist)
  (if (null? mylist)
      '()
      (append (my-reverse (cdr mylist))
              (list (car mylist)))))


; Question 3
(define (same-parity . x)
  (define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence))) ))
  (define (even x)
    (if (= 0 (modulo x 2))
        #t
        #f))
  (define (odd x)
    (if (= 1 (modulo x 2))
        #t
        #f))
  (cond ((even (car x))
         (filter even x))
        ((odd (car x))
         (filter odd x))))                 
  
  
; Question 4
(define (square-list items)
  (define (square x)
    (* x x))
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (define (square x)
    (* x x))
  (map (lambda (x) (square x))
       items))


; Question 5
(define (my-for-each func sequence)
  (cond ((not (null? sequence))
         (func (car sequence))
         (my-for-each func (cdr sequence)))))


; Question 6


; Question 7
(define (my-equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eqv? a b))
        ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))  
        (else #f)))

         
; Question 8A
(define (every? pred seq)
  (cond ((equal? seq '())
         #t)
        ((pair? seq)
         (and (pred (car seq))
              (every? pred (cdr seq))))
        (else #f)))


; Question 8B
; If the list is empty, 'every?' should return #t. If every element in seq is
; true for the predicate, 'every' should return true. The equivalence of this
; is that there exists not one element in the seq that does not satisfy the
; predicate. The empty list is a valid list and will evaluate to true for any
; predicate because it contains no elements that will cause the predicate to
; evaluate to false.


; Question 9
(define (unordered-union-set a b)
  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
  (define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
  (if (null? a)
      b
      (unordered-union-set (cdr a)
                           (adjoin-set (car a) b))))


; Question 10                
(define (ordered-union-set a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((= (car a) (car b))
         (cons (car a) (ordered-union-set (cdr a) (cdr b))))
        ((< (car a) (car b))
         (cons (car a) (ordered-union-set (cdr a) b)))
        ((> (car a) (car b))
         (cons (car b) (ordered-union-set a (cdr b))))))


; Question 11
(define (remove-val x seq)
  (cond ((null? seq) '())
        ((= x (car seq))
         (remove-val x (cdr seq)))
        (else (cons (car seq)
                    (remove-val x (cdr seq))))))