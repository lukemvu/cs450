;;; file: pi.scm
;;;
;;; This should be your second part of HW5.  All
;;; these definitions are from the textbook except cons-stream.

;;; cons-stream is defined (by a macro, as a special form). 

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (display-n stream n)
  (cond ((not (= 0 n))
         (display (stream-car stream))
         (newline)
         (display-n (stream-cdr stream) (- n 1)))))

(define (divisible? x y) (= (remainder x y) 0))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (number->list-of-digits integer)
  (map (lambda (x) (- (char->integer x) 48))
       (string->list (number->string integer))))

(define (list-of-digits->number list)
  (string->number
   (list->string
    (map (lambda (x) (integer->char (+ x 48))) list))))

(define (pow list)
  (define (pow-iter n sum)
    (if (= n 1)
        1
        (* 10 (pow-iter (- n 1) sum))))
  (if (> (length list) 1)
      (pow-iter (length list) 1)
      (display "Error: list less than 2")))

(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (list-to-stream (cdr lst)))))
      

; input list and list size, return prepend list
(define (prepend-list new-a-list old-list-size)
  (if (not (> (length new-a-list) old-list-size))
      (prepend-list (cons 0 new-a-list) old-list-size)
      new-a-list))

(define (produce m strm a a-list)
  (let* ((new-a (modulo a (pow a-list)))
         (new-a-list (cdr a-list)))
    (cons-stream (car a-list)
                 (mult-stream-iter m strm new-a new-a-list))))
                    
(define (consume m strm a a-list)
  (let* ((new-a (+ (* a 10)
                   (* m (stream-car strm))))
         (new-a-list (prepend-list
                      (number->list-of-digits new-a)
                      (length a-list))))
    (mult-stream-iter m (stream-cdr strm) new-a new-a-list)))

(define (append-a-list a-list)
  (if (null? a-list)
      the-empty-stream
      (cons-stream (car a-list) (append-a-list (cdr a-list)))))

(define (mult-stream-iter m strm a a-list)
    (cond ((stream-null? strm)
           (append-a-list a-list))
          ((and (> (length a-list) 0)
                (> (pow a-list)
                   (+ m (modulo a (pow a-list)))))
           (produce m strm a a-list))
          (else (consume m strm a a-list))))

(define (mult-stream m strm)
  (if (list? strm)
      (mult-stream-iter m (list-to-stream strm) 0 '())
      (mult-stream-iter m strm 0 '())))

; pi stuff here

; matrix definitions
(define (matrix a b c d) (list a b c d))
(define (select-a matrix) (car matrix))
(define (select-b matrix) (cadr matrix))
(define (select-c matrix) (caddr matrix))
(define (select-d matrix) (cadddr matrix))

; add two matricies
(define (add-matrix m0 m1)
  (list (+ (select-a m0)
           (select-a m1))
        (+ (select-b m0)
           (select-b m1))
        (+ (select-c m0)
           (select-c m1))
        (+ (select-d m0)
           (select-d m1))))

; multiply two matricies
(define (compose-matrix-o m0 m1)
  (list (+ (* (select-a m0) (select-a m1))
           (* (select-b m0) (select-c m1)))
        (+ (* (select-a m0) (select-b m1))
           (* (select-b m0) (select-d m1)))
        (+ (* (select-c m0) (select-a m1))
           (* (select-d m0) (select-c m1)))
        (+ (* (select-c m0) (select-b m1))
           (* (select-d m0) (select-d m1)))))

(define (compose-matrix m1 m0)
  (list (+ (* (select-a m0) (select-a m1))
           (* (select-b m0) (select-c m1)))
        (+ (* (select-a m0) (select-b m1))
           (* (select-b m0) (select-d m1)))
        (+ (* (select-c m0) (select-a m1))
           (* (select-d m0) (select-c m1)))
        (+ (* (select-c m0) (select-b m1))
           (* (select-d m0) (select-d m1)))))

; input stream
(define (add-matrix-streams s1 s2) (stream-map add-matrix s1 s2))
(define base-strm (cons-stream (matrix 1 4 0 2) base-strm))
(define original-strm (cons-stream
                       (matrix 1 6 0 3)
                       (add-matrix-streams base-strm original-strm)))
; display input stream
;(display-n original-strm 10)

; evaluate an lft
(define (eval-matrix matrix x)
  (quotient (+ (* x (select-a matrix))
               (select-b matrix))
            (+ (* x (select-c matrix))
               (select-d matrix))))

(define (eval-matrix-r matrix x)
  (/ (+ (* x (select-a matrix))
               (select-b matrix))
            (+ (* x (select-c matrix))
               (select-d matrix))))
                 
(define (pi)
  (define (pi-iter strm a)
    ;action
    (if (equal? (eval-matrix a 3) (eval-matrix a 4))
        ;produce
        (cons-stream (eval-matrix a 3)
                     (pi-iter strm (compose-matrix a
                                    (matrix 10 (* -10 (eval-matrix a 3)) 0 1)
                                    )))
        ;consume
        (pi-iter (stream-cdr strm) (compose-matrix (stream-car strm) a))))
  (pi-iter original-strm (matrix 1 0 0 1)))
