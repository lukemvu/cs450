(define expt-machine 
  (make-machine 
   '(b n r continue) 
   (list (list '= =) (list '* *) (list '- -)) 
   '((assign continue (label expt-done)) 
     expt-loop 
     (test (op =) (reg n) (const 0)) 
     (branch (label expt-base)) 
     (save continue) 
     (assign n (op -) (reg n) (const 1)) 
     (assign continue (label after-expt)) 
     (goto (label expt-loop)) 
     after-expt 
     (restore continue) 
     (assign r (op *) (reg r) (reg b)) 
     (goto (reg continue)) 
     expt-base 
     (assign r (const 1)) 
     (goto (reg continue)) 
     expt-done)))

(set-breakpoint expt-machine 'expt-loop 2)
(set-breakpoint expt-machine 'expt-loop 5)
(set-breakpoint expt-machine 'after-expt 1)


(set-register-contents! expt-machine 'b 2) 
(set-register-contents! expt-machine 'n 10) 
(start expt-machine) 
  
(get-register-contents expt-machine 'r)

(set-breakpoint expt-machine 'expt-loop 2)
(set-breakpoint expt-machine 'expt-loop 5)
(set-breakpoint expt-machine 'after-expt 1)










;;

(define expt-machine 
  (make-machine 
   '(b n r continue) 
   (list (list '= =) (list '* *) (list '- -)) 
   '((assign continue (label expt-done)) 
     expt-loop 
     (test (op =) (reg n) (const 0)) 
     (branch (label expt-base)) 
     (save continue) 
     (assign n (op -) (reg n) (const 1)) 
     (assign continue (label after-expt)) 
     (goto (label expt-loop)) 
     after-expt 
     (restore continue) 
     (assign r (op *) (reg r) (reg b)) 
     (goto (reg continue)) 
     expt-base 
     (assign r (const 1)) 
     (goto (reg continue)) 
     expt-done)))

;(set-breakpoint expt-machine 'expt-loop 2)
(set-breakpoint expt-machine 'expt-loop 4)
(set-breakpoint expt-machine 'after-expt 2)

(print-breakpoints expt-machine)

(cancel-breakpoint expt-machine 'after-expt 2)
(cancel-breakpoint expt-machine 'expt-loop 4)

(print-breakpoints expt-machine)

(start expt-machine)
(get-register-contents expt-machine 'r)




(define expt-machine 
  (make-machine 
   '(b n r continue) 
   (list (list '= =) (list '* *) (list '- -)) 
   '((assign continue (label expt-done)) 
     expt-loop 
     (test (op =) (reg n) (const 0)) 
     (branch (label expt-base)) 
     (save continue) 
     (assign n (op -) (reg n) (const 1)) 
     (assign continue (label after-expt)) 
     (goto (label expt-loop)) 
     after-expt 
     (restore continue) 
     (assign r (op *) (reg r) (reg b)) 
     (goto (reg continue)) 
     expt-base 
     (assign r (const 1)) 
     (goto (reg continue)) 
     expt-done)))

(set-breakpoint expt-machine 'expt-loop 2)
(set-breakpoint expt-machine 'expt-loop 4)
(set-breakpoint expt-machine 'after-expt 1)

(print-breakpoints expt-machine)

(newline)
(newline)

(cancel-breakpoint expt-machine 'after-expt 1)
(cancel-breakpoint expt-machine 'expt-loop 4)

(print-breakpoints expt-machine)


#|        
        ;; Cancel breakpoint
        (define (cancel-breakpoint label n)
          ;; Search for label n in break-point list and null out value
          (define (cancel-break-point-helper label n bp-list)
            (cond ((null? bp-list)
                   (newline))
                   ((and (equal? label (cadar bp-list))
                        (equal? n (cddar bp-list)))
                   ; sets label and n to false
                   (set-car! (cdar bp-list) #f)
                   (set-cdr! (cdar bp-list) #f)
                   ; sets instruction pair to false
                   (set-car! (caar bp-list) #f)
                   ;(set-cdr! (caar bp-list) #f) ;this broke something
                   )
                  (else
                   (cancel-break-point-helper label n (cdr bp-list)))))
          (cancel-break-point-helper label n breakpoint-list))
|#