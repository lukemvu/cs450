(define (lookup key table)
  (let ((record (assoc-r key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc-r key records)
               (cond ((null? records) false)
                     ((equal? key (caar records)) (car records))
                      (else (assoc-r key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list 'stuffer))

(define stuff (make-table))