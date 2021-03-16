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
            (else ((print "Unknown request -- MAKE-ACCOUNT"
                          m)))))))

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
            (else ((print "Unknown request -- MAKE-ACCOUNT"
                          m)))))))

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
              (else ((print "Unknown request -- MAKE-ACCOUNT"
                            m))))))))

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
  (lambda (password balance)
    (lambda (p m)
      (lambda (amount)
        (cond ((not (equal? password p)) (lambda (x) "Incorrect password"))
              ((eq? m 'withdraw)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))
              ((eq? m 'deposit)
               (set! balance (+ balance amount))
               balance)
              (else ((print "Unknown request -- MAKE-ACCOUNT"
                            m))))))))

; QUESTION 5
