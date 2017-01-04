(include "ass3.scm")

;;----------------------------------------------------------------------------------    part 5





(define set?
	(lambda (expr v)
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
			  ((and (equal? (car expr) 'lambda-simple) 
			  	   (not (member-simple v (get-lambda-simple-param expr)))) (set? (get-lambda-simple-body expr) v))
			  ((and (equal? (car expr) 'lambda-opt) 
			  	   (not (member-opt v (get-lambda-opt-param expr)))) (set? (get-lambda-opt-body expr) v))
			  ((and (equal? (car expr) 'lambda-var) 
			  	   (not (equal? v (get-lambda-var-param expr)))) (set? (get-lambda-var-body expr) v))
			  (else #f))
		))



(define to-box?;;; fix
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

; in this point l-expr is lambda-expression and v is a variable that needs boxing
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