;; machine has already been defined

;; set contents

;; set breakpoints

;; run machine

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))


(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 240)

(set-breakpoint gcd-machine 'test-b 4)

(start gcd-machine)

(get-register-contents gcd-machine 't)
(proceed-machine gcd-machine)
(get-register-contents gcd-machine 'a)

(set-register-contents! gcd-machine 'a 40)
(set-breakpoint gcd-machine 'test-b 1)
(cancel-breakpoint gcd-machine 'test-b 4)
(proceed-machine gcd-machine)
(get-register-contents gcd-machine 'a)
(begin
  (set-breakpoint gcd-machine 'test-b 4)
  (cancel-all-breakpoints gcd-machine)
  (proceed-machine gcd-machine))