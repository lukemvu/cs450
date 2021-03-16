; header
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (divisible? x y) (= (remainder x y) 0))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))


; part 1 question 1
(define (display-n stream n)
  (cond ((not (= 0 n))
         (display (stream-car stream))
         (newline)
         (display-n (stream-cdr stream) (- n 1)))))


;part 1 question 2 ex 3.50 p 324
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;part 1 question 3
(define notdiv-235
  (stream-filter (lambda (x)
                   (and
                    (not (divisible? x 2))
                    (not (divisible? x 3))
                    (not (divisible? x 5))))
                 integers))