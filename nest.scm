(include "box.scm")


;@param lst - list of expression from a lambda's body with seq
;@example - (lambd-simple (x) (seq (a b c))) --> lst = (a b c)
(define make-def-set-list
	(lambda (lst)
		(cond ((null? lst) lst)
			  ((equal? (caar lst) 'def) (cons `(set ,(get-def-var (car lst)) ,(get-def-val (car lst))) (make-def-set-list (cdr lst))))
			  (else '()))
		))

;@param lst - list of expression from a lambda's body with seq
;@example - (lambd-simple (x) (seq (a b c))) --> lst = (a b c)
(define make-def-var-list
	(lambda (lst)
		(cond ((null? lst) lst)
			  ((equal? (caar lst) 'def) (cons (cadr (get-def-var (car lst))) (make-def-var-list (cdr lst))))
			  (else '()))
		))

;@param lst - list of expression from a lambda's body with seq
;@example - (lambd-simple (x) (seq (a b c))) --> lst = (a b c)
(define make-rest-body
	(lambda (lst)
		(cond ((null? lst) lst)
			   ((equal? (caar lst) 'def) (make-rest-body (cdr lst)))
			   (else lst))
		))

(define make-false-list
	(lambda (var-list)
		(if (null? var-list)
			var-list
			(cons `(const #f) (make-false-list (cdr var-list))))
		))

(define more-then-one?
	(lambda (lst)
		(cond ((null? lst) #f)
			  ((null? (cdr lst)) #f)
			  (else #t))
		))

(define eliminate
	(lambda (l-body)
;(disp l-body)
	(if (equal? (car l-body) 'seq)
		(let* ((var-list (make-def-var-list (get-seq-list l-body)))
			   (set-list (make-def-set-list (get-seq-list l-body)))
			   (rest-body (make-rest-body (get-seq-list l-body)))
			   (false-list (make-false-list var-list))
			   (new-body `(,@set-list ,@rest-body)))
			(cond ((null? set-list) l-body) 
				  ((more-then-one? new-body) `(applic (lambda-simple ,var-list (seq ,new-body)) ,false-list))
				  (else  `(applic (lambda-simple ,var-list ,new-body) ,false-list))))
		(let* ((var-list (make-def-var-list `(,l-body)))
			   (set-list (make-def-set-list `(,l-body)))
			   (rest-body (make-rest-body `(,l-body)))
			   (false-list `(,l-body))
			   (new-body `(,l-body)))
			(cond ((null? set-list) l-body) 
				  ((more-then-one? new-body) `(applic (lambda-simple ,var-list (seq ,new-body)) ,false-list))
				  (else  `(applic (lambda-simple ,var-list ,new-body) ,false-list)))))
	))   



(define eliminate-nested-defines
	(lambda (expr)
		;(disp expr)
		(cond ((equal? (car expr) 'set) `(set ,(eliminate-nested-defines (get-set-var expr))
											  ,(eliminate-nested-defines (get-set-val expr))))
			  ((equal? (car expr) 'if3) `(if3 ,(eliminate-nested-defines (get-if-test expr))
			  								  ,(eliminate-nested-defines (get-if-dit expr))
			  								  ,(eliminate-nested-defines (get-if-dif expr))))
			  ((equal? (car expr) 'def) `(def ,(eliminate-nested-defines (get-def-var expr))
											  ,(eliminate-nested-defines (get-def-val expr))))
			  ((equal? (car expr) 'applic) `(applic ,(eliminate-nested-defines (get-applic-operator expr))
			  										,(map eliminate-nested-defines (get-applic-operands expr))))
			  ((equal? (car expr) 'or) `(or ,(map eliminate-nested-defines (get-or-body expr))))
			  ((equal? (car expr) 'seq) `(seq ,(map eliminate-nested-defines (get-seq-list expr))))
			  ((equal? (car expr) 'lambda-simple)  `(lambda-simple ,(get-lambda-simple-param expr) 
			  											,(eliminate-nested-defines (eliminate (get-lambda-simple-body expr)))))
			  ((equal? (car expr) 'lambda-opt) `(lambda-opt ,(get-lambda-opt-param expr) ,(get-lambda-opt-param-list expr)
			  											,(eliminate-nested-defines (eliminate (get-lambda-opt-body expr)))))
			  ((equal? (car expr) 'lambda-var)  `(lambda-var ,(get-lambda-var-param expr) 
			  											,(eliminate-nested-defines (eliminate (get-lambda-var-body expr)))))
			  (else expr))
		))







;;assignment sections:
;;3- eliminate-nested-defines
;;4- remove-applic-lambda-nil
;;5- box-set
;;6- pe->lex-pe
;;7- annotate-tc
	
(define run
  (lambda (expr sec)
    (cond ((eq? sec 3) (eliminate-nested-defines expr))
          ((eq? sec 4) (remove-applic-lambda-nil expr))
          ((eq? sec 5) (box-set expr))
          ((eq? sec 6) (pe->lex-pe expr))
          (else (annotate-tc expr))
          )))



(define run-sq
   (lambda (expr sec)
	  (cond ((eq? sec 3) (run expr 3))
	  		((eq? sec 4) (run (run expr 3) 4))
	  		((eq? sec 5) (run (run (run expr 3) 4) 5))
	  		((eq? sec 6) (run (run (run (run expr 3) 4) 5) 6))
	  		(else (run (run (run (run (run expr 3) 4) 5) 6) 7)))

  
 

		))