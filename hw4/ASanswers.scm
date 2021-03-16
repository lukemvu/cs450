;; HW4 - part 1 and optimal BST
;;; Answer part 1 here.
(define (error x y) (display "Error: ") (display x) (display ": ") (display y))

; QUESTION 1A
(define make-account-lambda
  (lambda (balance)
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
        balance))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else ((display "Unknown request -- MAKE-ACCOUNT"
                          )))))))

; QUESTION 1B
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds")))
            ((eq? m 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
            (else ((display "Unknown request -- MAKE-ACCOUNT"
                          )))))))

; QUESTION 1C
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
        (cond ((eq? m 'withdraw)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))
              ((eq? m 'deposit)
               (set! balance (+ balance amount))
               balance)
              (else ((display "Unknown request -- MAKE-ACCOUNT"
                            ))))))))

; QUESTION 3
(define (make-monitored f)
  (define counter 0)
  (define (dispatch m)
    (cond ((equal? m 'how-many-calls?) counter)
          ((equal? m 'reset-count) (set! counter 0))
          (else (set! counter (+ counter 1)) (f m))))
  dispatch)

; QUESTION 4
(define make-pw-account
  (lambda (balance password)
    (lambda (p m)
      (cond ((not (equal? p password)) (lambda (x) "Incorrect password"))
            ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds")))
            ((eq? m 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
            (else ((display "Unknown request -- MAKE-ACCOUNT"
                            )))))))

;(define luke (make-pw-account 100 'nice))

;; Part 2 starts here! 
;; read-file produces a list whose elements are the expressions in the file.
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;(define data-small (with-input-from-file "keys-small.dat" read-file))
;(define data-medium (with-input-from-file "keys-medium.dat" read-file))
;(define data-large (with-input-from-file "keys-large.dat" read-file))

(define (entry tree) (car tree))
(define (key entry) (car entry))
(define (freq entry) (cadr entry))
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 


(define (make-tree entry left right) 
  (list entry left right)) 
  
(define (adjoin-set x set) 
  (cond ((null? set) (make-tree x '() '()))
        (else
         (let ((rkey (car (entry set))))
           (cond ((= (car x) rkey) set) 
                 ((< (car x) rkey)
                  (make-tree (entry set) 
                             (adjoin-set x (left-branch set)) 
                             (right-branch set))) 
                 ((> (car x) rkey) 
                  (make-tree (entry set) 
                             (left-branch set) 
                             (adjoin-set x (right-branch set))))))))) 
;; A skeleton for BST. You may add things here 
(define (make-table) 
  (let ((local-table '())) 
      
    (define (lookup key records) 
      (if (null? records) #f
          (let ((rkey (car (entry records))))
            (cond ((= key rkey) (entry records)) 
                  ((< key rkey) (lookup key (left-branch records))) 
                  ((> key rkey) (lookup key (right-branch records))))))) 
      
    (define (insert! dat)
      (let ((rkey (car dat)))
        (let ((record (lookup rkey local-table))) 
          (if record 
              (set-cdr! record (freq dat)) 
              (set! local-table (adjoin-set dat local-table)))))) 
      
    (define (get key) 
      (lookup key local-table))
     
    ;build table from data. Data is a list of (key freq)
    (define (build-tree-from-data the-keys)
      (if (null? the-keys) 'ok
          (begin (insert! (car the-keys))
                 (build-tree-from-data (cdr the-keys)))))
     
    (define (dispatch m) 
      (cond ((eq? m 'get-proc) get) 
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'build-proc) build-tree-from-data)
            ((eq? m 'print) local-table) 
            (else (display "Undefined operation -- TABLE" )))) 
    dispatch))
  
;(define table (make-table)) 
;(define get (table 'get-proc)) 
;(define put (table 'insert-proc))
;(define cost (table 'cost-proc))

;;; Put your naive and DP procedures here.

(define (min-cost-naive list)
  ;splitting list around k
  (define (split-left list k)
    (if (equal? (car list) k)
        '()
        (cons (car list) (split-left (cdr list) k))))
  (define (split-right list k)
    (if (equal? (car list) k)
        (cdr list)
        (split-right (cdr list) k)))
  ;sum of weights in list
  (define (sum-weight list)
    (define (sum-list-iter list1 sum)
      (if (null? list)
          sum
          (+ (+ sum (cadr (car list1)))
             (sum-weight (cdr list1)))))
    (sum-list-iter list 0))
  (define (min-cost-naive-iter list k min)
    ;empty list return 0
    (cond ((null? list) 0)
          ;last element of list, return weight
          ((null? (cdr list)) (cadr (car list)))
          ; index reach max, return min
          ((= (length list) k) min)
          (else (let* ((weight (sum-weight list))
                       ;pick some pair k at current index
                       (root (list-ref list k))
                       (left (split-left list root))
                       (right (split-right list root))
                       (min-cost-left (min-cost-naive-iter left 0 10000))
                       (min-cost-right (min-cost-naive-iter right 0 10000))
                       (sum-comp (+ weight
                                    min-cost-left
                                    min-cost-right)))
                  (cond ((> min sum-comp)
                         ;for each key
                         ;call on next index k
                         (min-cost-naive-iter list (+ k 1) sum-comp))
                        (else (min-cost-naive-iter list (+ k 1) min)))))))
  (min-cost-naive-iter list 0 10000))

;(define a '((1 10) (2 20) (3 30) (4 40) (5 50)))
;(define b '((10 34) (12 8) (20 50)))
;(define c '((1 0.05) (2 0.15) (3 0.1) (5 0.05) (7 0.05) (8 0.1) (10 0.2) (11 0.1) (13 0.1) (14 0.06) (16 0.1)))

;;;; the following procedure is handy for timing things
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

(define (timed f . args)
  (let ((init (runtime)))
    (let ((v (apply f args)))
      (display (list 'time: (- (runtime) init)))
      (newline)
      v)))

