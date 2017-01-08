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
			(cons #f (make-false-list (cdr var-list))))
		))

(define more-then-one?
	(lambda (lst)
		(cond ((null? lst) #f)
			  ((null? (cdr lst)) #f)
			  (else #t))
		))

(define eliminate
	(lambda (l-body)

		(if (equal? (car l-body) 'seq)
			`(applic (lambda-simple ,(make-def-var-list (get-seq-list l-body)) 
								(seq (,@(make-def-set-list (get-seq-list l-body))
								     ,@(make-rest-body (get-seq-list l-body))))) ,@(make-false-list (get-seq-list l-body))) 
			(let ((single-expr-body `(,l-body)))
				`(applic (lambda-simple ,(make-def-var-list single-expr-body) 
								(seq (,@(make-def-set-list single-expr-body)
								     ,@(make-rest-body single-expr-body)))) ,@(make-false-list single-expr-body))))

		))   



				


(define eliminate-nested-defines
	(lambda (expr)
		(cond ((equal? (car expr) 'set) `(set ,(eliminate-nested-defines (get-set-var expr))
											  ,(eliminate-nested-defines (get-set-val expr))))
			  ((equal? (car expr) 'if3) `(if3 ,(eliminate-nested-defines (get-if-test expr))
			  								  ,(eliminate-nested-defines (get-if-dif expr))
			  								  ,(eliminate-nested-defines (get-if-dif expr))))
			  ((equal? (car expr) 'def) `(set ,(eliminate-nested-defines (get-def-var expr))
											  ,(eliminate-nested-defines (get-def-val expr))))
			  ((equal? (car expr) 'applic) `(applic ,(eliminate-nested-defines (get-applic-operator expr))
			  										,(map eliminate-nested-defines (get-applic-operands expr))))
			  ((equal? (car expr) 'or) `(or ,(map eliminate-nested-defines (get-or-body expr))))
			  ((equal? (car expr) 'seq) `(or ,(map eliminate-nested-defines (get-seq-list expr))))
			  ((equal? (car expr) 'lambda-simple)  `(lambda-simple ,(get-lambda-simple-param expr) 
			  											,(eliminate-nested-defines (eliminate (get-lambda-simple-body expr)))))
			  ((equal? (car expr) 'lambda-opt) `(lambda-opt ,(get-lambda-opt-param expr) ,(get-lambda-opt-param-list expr)
			  											,(eliminate-nested-defines (eliminate (get-lambda-opt-body expr)))))
			  ((equal? (car expr) 'lambda-var)  `(lambda-var ,(get-lambda-var-param expr) 
			  											,(eliminate-nested-defines (eliminate (get-lambda-var-body expr)))))
			  (else expr))
		))








