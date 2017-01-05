(include "ass3.scm")

;;----------------------------------------------------------------------------------    part 5



(define set?
	(lambda (expr v)  ;first iteration with original lambda body only
	;	(display "set?\n")
		(cond ((equal? (car expr) 'set) (if (equal? (get-set-var expr) `(var ,v))
											#t
											#f))
			  ((equal? (car expr) 'if3) (or (set? (get-if-test expr) v) 
			  								(set? (get-if-dit expr) v) 
			  								(set? (get-if-dif expr) v)))
			  ((equal? (car expr) 'def) (or (set? (get-def-var expr) v) 
			  								(set? (get-def-val expr) v)))
			  ((equal? (car expr) 'applic) (or (set? (get-applic-operator expr) v)
			  								   (ormap (lambda (ex) (set? ex v)) (get-applic-operands expr))))
			  ((equal? (car expr) 'seq) (ormap (lambda (ex) (set? ex v)) (get-seq-list expr)))
			  ((equal? (car expr) 'or) (ormap (lambda (ex) (set? ex v)) (get-or-body expr)))
			  ((and (is-lambda-expr? expr) (not (member-param? expr v))) (set? (get-lambda-body expr) v))			  	   
			  (else #f))
		))

(define read?
	(lambda (expr v)  ;check for expr=(var v) in nested scope
	;	(display "read?\n")
		(cond ;((equal? (car expr) 'set) (or (read? (get-set-var expr) v) (read? (get-set-val expr) v)))
			  ((equal? (car expr) 'set) (read? (get-set-val expr) v))
			  ((equal? (car expr) 'var) (equal? expr `(var ,v)))
			  ((equal? (car expr) 'if3) (or (read? (get-if-test expr) v) 
			  								(read? (get-if-dit expr) v) 
			  								(read? (get-if-dif expr) v)))
			  ((equal? (car expr) 'def) (or (read? (get-def-var expr) v) 
			  								(read? (get-def-val expr) v)))
			  ((equal? (car expr) 'applic) (or (read? (get-applic-operator expr) v)
			  								   (ormap (lambda (ex) (read? ex v)) (get-applic-operands expr))))
			  ((equal? (car expr) 'seq) (ormap (lambda (ex) (read? ex v)) (get-seq-list expr)))
			  ((equal? (car expr) 'or) (ormap (lambda (ex) (read? ex v)) (get-or-body expr)))
			  ((and (is-lambda-expr? expr) (not (member-param? expr v))) (read? (get-lambda-body expr) v))			  	   
			  (else #f))
		))


(define bound?
	(lambda (expr v)
	;	(display "bound?\n")
		(cond ((equal? (car expr) 'set) (or (bound? (get-set-var expr) v) (bound? (get-set-val expr) v)))
			  ((equal? (car expr) 'if3) (or (bound? (get-if-test expr) v) 
			  								(bound? (get-if-dit expr) v) 
			  								(bound? (get-if-dif expr) v)))
			  ((equal? (car expr) 'def) (or (bound? (get-def-var expr) v) 
			  								(bound? (get-def-val expr) v)))
			  ((equal? (car expr) 'applic) (or (bound? (get-applic-operator expr) v)
			  								   (ormap (lambda (ex) (bound? ex v)) (get-applic-operands expr))))
			  ((equal? (car expr) 'seq) (ormap (lambda (ex) (bound? ex v)) (get-seq-list expr)))
			  ((equal? (car expr) 'or) (ormap (lambda (ex) (bound? ex v)) (get-or-body expr)))
			  ((and (is-lambda-expr? expr) (not (member-param? expr v))) (read? expr v))		  	   
			  (else #f))           
	))







(define to-box?
	(lambda (l-expr v)	
	;	(display "to-box?\n")
         (and (read? (get-lambda-body l-expr) v) (set? (get-lambda-body l-expr) v) (bound? (get-lambda-body l-expr) v))
		))

(define box-var  
	(lambda (expr v)
	;	(display "box-var\n")
		(cond ((null? expr) expr)
		      ((atom? expr) expr)
		      ((equal? expr `(var ,v)) `(box-get (var ,v)))
		      ((and (equal? (car expr) 'set) (equal? (cadr expr) `(var ,v))) `(box-set ,(cadr expr) ,(box-var (caddr expr) v)))
              ((and (is-lambda-expr? expr) (member-param? expr v)) expr)               
		      (else (map (lambda (ex) (box-var ex v)) expr)))

		))


(define box-simple 
	(lambda (l-expr v minor)  
	;	(display "box-simple\n")
		(let* ((l-type (car l-expr))
			   (args (cadr l-expr))
			   (rest (caddr l-expr)))
			(if (equal? (car rest) 'seq)
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,@(box-var (cadr rest) v))))
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,(box-var rest v)))))
			)
		))

(define box-opt 
	(lambda (l-expr v minor)  
	;	(display "box-opt\n")
		(let* ((l-type (car l-expr))
			   (args (get-lambda-opt-param l-expr))
			   (lst (get-lambda-opt-param-list l-expr))
			   (rest (get-lambda-body l-expr)))
			(if (equal? (car rest) 'seq)
				`(,l-type ,args lst (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,@(box-var (cadr rest) v))))
				`(,l-type ,args lst (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,(box-var rest v)))))
			)
		))

(define box-variadic
	(lambda (l-expr v minor)  
	;	(display "box-var\n")
		(let* ((l-type (car l-expr))
			   (args (get-lambda-var-param l-expr))
			   (rest (get-lambda-body l-expr)))
			(if (equal? (car rest) 'seq)
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,@(box-var (cadr rest) v))))
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,(box-var rest v)))))
			)
		))




(define box
  (lambda (l-expr v minor)
  ;	(display "box \n")
     (cond  ((equal? (car l-expr) 'lambda-simple) (box-simple l-expr v minor))
            ((equal? (car l-expr) 'lambda-opt) (box-opt l-expr v minor))
            (else  (equal? (caaddr l-expr) (box-variadic l-expr v minor))))))




(define box-all-args
	(lambda (l-expr args minor)
	;	(display "box-all-args\n")
		(cond ((null? args) l-expr)
		      ((to-box? l-expr (car args)) (box-all-args (box l-expr (car args) (+ minor 1)) (cdr args) (+ minor 1)))
		      (else (box-all-args l-expr (cdr args) (+ minor 1))))
		))

(define box-all
	(lambda (l-expr)
	;	(display "box-all\n")
          (cond ((equal? (car l-expr) 'lambda-simple) (box-all-args l-expr (get-lambda-simple-param l-expr) -1))
                ((equal? (car l-expr) 'lambda-opt) (box-all-args l-expr `(,@(get-lambda-opt-param l-expr) ,(get-lambda-opt-param-list l-expr)) -1))
                (else (box-all-args l-expr `(,(get-lambda-opt-param-list l-expr)) -1)))
		))

(define box-set
  (lambda (expr)
  ;	(display "box-set\n")

       (cond ((null? expr) expr)
             ((atom? expr) expr)
             ((is-lambda-expr? expr) (map box-set (box-all expr)))
             (else (map box-set expr)))                               
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