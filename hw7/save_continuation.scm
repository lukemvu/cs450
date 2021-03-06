(define call/cc call-with-current-continuation)

(define target '())

;;; Define a procedure which needs to escape. Use the target to tell
;;; it where to escape to.

(define (f x)
  (cond ((= x 0)
         (display "0 entered; try again.")
         (newline)
         (target x)) ;;; the argument x will be ignored.
        (else
         (display "Success: ")
         (display x)
         (newline))
        )
  )

;;; Define the calling routine, which in turn defines the target.
(define (main_loop)
  (call/cc
   (lambda(here)
     (set! target here)))
  (display "Type a number different from 0: ")
  (let ((n (read)))
    ;; First check to make sure that a number was entered.
    (if (not (number? n))
        (begin
          (display n)
          (display " is not a number; try again.")
          (newline)
          (target n) ;;; the argument n will be ignored.
          )
        )
    ;; OK; a number was entered. Now call f to do the rest of the
    ;; processing.
    (f n)
    )
  (main_loop)
  )

(main_loop)