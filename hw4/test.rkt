(define data '((10 34) (12 8) (20 50)))

(define (get-left-subtree lst root)
  (cond((equal? lst '()) '())
       ((equal? (list-ref lst 0) root) '())
       (else (cons (car lst)
                   (get-left-subtree (cdr lst) root)))))

(define (get-right-subtree lst root)
  (cond ((equal? lst '()) '())
        ((= (caar lst) (car root)) (cdr lst))
        (else (get-right-subtree (cdr lst) root))))

(define (sum-tree lst)
  (if (null? lst) 0
      (+ (car (cdar lst)) (sum-tree (cdr lst)))))

(define (min-cost-iter lst k min)
  (cond ((equal? lst '()) 0)
        ((null? lst) 0)
        ((equal? (length lst) 1) (car(cdar lst)))
        ((= k 0) min)
        (else (let* ((root (list-ref lst (- k 1)))
                     (left-tree (get-left-subtree lst root))
                     (right-tree (get-right-subtree lst root))
                     (sum (sum-tree lst))
                     (min-left (min-cost-iter left-tree (length left-tree) 1000000))
                     (min-right (min-cost-iter right-tree (length right-tree) 1000000))
                     (sum-iter (+ sum
                                  min-left
                                  min-right
                                  )))
                (cond ((> min sum-iter) (min-cost-iter lst (- k 1) sum-iter))
                      (else (min-cost-iter lst (- k 1) min)))))))
 
(define (min-cost-naive list)
  (min-cost-iter list (length list) 1000000))
