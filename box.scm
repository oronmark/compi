(include "ass3.scm")

;;----------------------------------------------------------------------------------    part 5



(define set?
	(lambda (expr v)  ;first iteration with original lambda body only
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
			  ((and (is-lambda-expr? expr) (not (member-param? expr v))) (set? (get -get-lambda-body expr) v))			  	   
			  (else #f))
		))

(define read?
	(lambda (expr v)  ;check for expr=(var v) in nested scope
		(cond ((equal? (car expr) 'set) (or (read? (get-set-var expr) v) (read? (get-set-val expr) v)))
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
			  ((and (is-lambda-expr? expr) (not (member-param? expr v))) (read? (get -get-lambda-body expr) v))			  	   
			  (else #f))
		))





(define to-box?
	(lambda (l-expr v)
          (and (read? (get-lambda-body l-expr)) (set? (get-lambda-body l-expr)) (bound? (get-lambda-body l-expr)))
		))

(define box-var  ;;need to fix this
	(lambda (expr v)
		(cond ((null? expr) expr)
		      ((atom? expr) expr)
		      ((equal? expr `(var ,v)) `(box-get (var ,v)))
		      ((and (equal? (car expr) 'set) (equal? (cadr expr) `(var ,v))) `(box-set ,(cadr expr) ,(box-var (caddr expr) v)))
                 ((and (equal? (car expr) 'lambda-simple) (member-simple? v expr)) expr)
                 ((and (equal? (car expr) 'lambda-opt) (member-opt? v expr)) expr)
                 ((and (equal? (car expr) 'lambda-var) (equal? v (cadr expr))) expr)                 
		      (else (map (lambda (ex) (box-var ex v)) expr)))

		))


(define box  
	(lambda (l-expr v minor)  
		(let* ((l-type (car l-expr))
			   (args (cadr l-expr))
			   (rest (caddr l-expr)))
			(if (equal? (car rest) 'seq)
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,@(box-var (cadr rest) v))))
				`(,l-type ,args (seq ((set (pvar ,v ,minor) (box (pvar ,v ,minor))) ,(box-var rest v)))))
			)
		))

(define box-all-args
	(lambda (l-expr args minor)
		(cond ((null? args) l-expr)
		      ((to-box? l-expr (car args)) (box-all-args (box l-expr (car args) (+ minor 1)) (cdr args) (+ minor 1)))
		      (else (box-all-args l-expr (cdr args) (+ minor 1))))
		))

(define box-all
	(lambda (l-expr)
          (cond ((equal? (car l-expr) 'lambda-simple) (box-all-args l-expr (get-lambda-simple-param l-expr) -1))
                ((equal? (car l-expr) 'lambda-opt) (box-all-args l-expr `(,@(get-lambda-opt-param l-expr) ,(get-lambda-opt-param-list l-expr)) -1))
                (else (box-all-args l-expr `(,(get-lambda-opt-param-list expr)) -1)))
		))

(define box-set
  (lambda (expr)
       (cond ((null? expr) expr)
             ((atom? expr) expr)
             ((is-lambda-expr? expr) (map box-set (box-all expr)))
             (else (map box-set expr)))                               
    ))





(define expr (parse 
	'(lambda (a)
		(set! a 3)
		(lambda (y)
			y
			(a operand)))))