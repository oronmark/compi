(define flatten-one-level
	(lambda (lst)
		(cond ((or (null? lst) (atom? lst)) '())
			  ((atom? (car lst)) (flatten-one-level (cdr lst)))
			  (else (append (car lst) (flatten-one-level (cdr lst)))))		
		))

;; returns a list of expr sub-expressions
(define expr-tree
	(lambda (expr)
		(cond ((atom? expr) expr)
			   ((equal? (car expr) 'quote) `(,expr))
			   (else `(,expr ,@(flatten-one-level (map expr-tree expr)))))
		))

(define member? 
	(lambda (elem lst)
		(ormap (lambda (x) (equal? elem x)) lst)					
		))

(define dup 
	(lambda (lst)
	  (cond ((null? lst) lst)
	  		((atom? (car lst)) (dup (cdr lst)))
	  		((equal? (caar lst) 'quote) (dup (cdr lst)))
	        ((member (car lst) (cdr lst)) (cons (car lst) (dup (remove (car lst) lst))))
	        (else (dup (cdr lst))))
	  ))

(define nest-level
	(lambda (lst)
		(cond ((atom? lst) 0)
			  (else (+ 1 (apply max (map nest-level lst)))))
		))

(define assign-nest-level
	(lambda (lst)
		(sort (lambda (v u) (< (cdr v) (cdr u))) (map (lambda (sub) (cons sub (nest-level sub))) lst))
		))

(define remove-nested-number
	(lambda (lst)
		(if (null? lst)
			lst
			(cons (caar lst) (remove-nested-number (cdr lst))))

		))

(define swap-name
	(lambda (lst pair)
		(if (or (null? lst) (atom? lst))
			lst
			(if (equal? lst (car pair)) 
				(cdr pair)
				(map (lambda (x) 
					(cond ((or (null? x) (atom? x)) x)
						  ((equal? x (car pair)) (cdr pair))
						  (else (swap-name x pair))))           lst)
			)
		)			 
	))

(define assign-names
	(lambda (dupList)
		(if (null? dupList)
			dupList
			(let ((first (car dupList))
			      (name (gensym)))
		     	(cons (cons first name) (assign-names (swap-name (cdr dupList) (cons first name)))))	   
			)		
		))

(define make-let-ribs
	(lambda (dups)
			(map (lambda (dup) (cons (cdr dup) `(,(car dup)))) dups)
		))

(define get-name
	(lambda (sub ribs)
		(cond ((null? ribs) #f)
			   ((equal? (cadar ribs) sub) (caar ribs))
			   (else (get-name sub (cdr ribs))) 
			)
		))

(define get-sub-index-helper
	(lambda (sub dups n)
		(cond ((null? dups) #f)
			  ((equal? (car dups) sub) n)
			  (else (get-sub-index-helper sub (cdr dups) (+ 1 n))))
		))

(define get-sub-index
	(lambda (sub dups)
		(get-sub-index-helper sub dups 0)))

(define get-index-name
	(lambda (index ribs)
		(if (eq? index 0) 
			(caar ribs)
			(get-index-name (- index 1) (cdr ribs)))
		))


(define make-new-expr
	(lambda (expr ribs dups)
		(if (atom? expr)
			expr
			(map (lambda (sub)
						(cond 
							  ((get-sub-index sub dups)  (get-index-name (get-sub-index sub dups) ribs))
							  (else (make-new-expr sub ribs dups))
							)) expr)
			 )
		))


(define remove-n
	(lambda (elem lst n)
		(cond ((or (null? lst) (eq? n 0)) lst)
			   ((equal? elem (car lst)) (remove-n elem (cdr lst) (- n 1)))
			   (else (cons (car lst) (remove-n elem (cdr lst) n)))
			)
		))


(define keep-dup?
	(lambda (new-expr name)
			(ormap (lambda (sub)
						(cond ((atom? sub) (equal? sub name))
							(else (keep-dup? sub name)))) new-expr)
		))



(define minimize-dups
	(lambda (dups ribs new-expr)
		(cond ((null? dups) dups)
			  ((keep-dup? new-expr (caar ribs))
			  					(cons (car dups) (minimize-dups (cdr dups) (cdr ribs) new-expr)))
			  (else (minimize-dups (cdr dups) (cdr ribs) new-expr)))
		))

(define dup-in-dup? 
	(lambda (small big n)
		(cond ((atom? big) #f)
			  ((and (equal? (car big) small) (eq? n 1)) #t)
			  ((and (equal? (car big) small) (eq? n 0)) (dup-in-dup? small (cdr big) 1))
			  (else (or (dup-in-dup? small (car big) 0) (dup-in-dup? small (cdr big) 0))))

				
		))

;return #t if dup is a duplicated expression in one of min-dups members, #f otherwise
(define keep-dup-expend?
	(lambda (dup min-dups)
		(cond ((null? min-dups) #f)
			  (else (or (dup-in-dup? dup (car min-dups) 0) (keep-dup-expend? dup (cdr min-dups)))))
		))


(define expend-dups
	(lambda (dups min-dups)
		(cond ((null? dups) dups)
			   ((keep-dup-expend? (car dups) min-dups) (cons (car dups) (expend-dups (cdr dups) min-dups)))
			   (else (expend-dups (cdr dups) min-dups)))
		))

;;returns list1-list2
(define sub-list 
	(lambda (lst1 lst2)
		(cond ((null? lst1) lst1)
			  ((member? (car lst1) lst2) (sub-list (cdr lst1) lst2))
			  (else (cons (car lst1) (sub-list (cdr lst1) lst2))))

		))

(define cse
	(lambda (expr)
		(let* ((dups (remove-nested-number (assign-nest-level (dup (expr-tree expr)))))
			   (ribs (make-let-ribs (assign-names dups)))
			   (new-expr (make-new-expr expr ribs dups))
			   (min-dups (minimize-dups dups ribs new-expr))
			   (min-ribs (make-let-ribs (assign-names min-dups)))
			   (min-expr (make-new-expr expr min-ribs min-dups))
			   (rest-non-min-dups (sub-list dups min-dups))
			   (final-dups (append (expend-dups rest-non-min-dups min-dups) min-dups))
			   (final-ribs (make-let-ribs (assign-names final-dups)))
			   (final-expr (make-new-expr expr final-ribs final-dups))
			   )

			   		 (print-gensym #f)					
				  (if (null? dups)
			   		 	expr
			   			(if (> (length final-ribs) 1)
			   		 	`(let* ,final-ribs ,final-expr)
			   		 	`(let ,final-ribs ,final-expr)))
			
			   		 ;  (if (null? dups)
			   		 ; 	expr
			   			; (if (> (length min-ribs) 1)
			   		 ; 	`(let* ,min-ribs ,min-expr)
			   		 ; 	`(let ,min-ribs ,min-expr)))

			   		 ; (display "dups: ")  (display dups)
			   		 ; (display "\n")
			   		 ; (display "min: ")  (display min-dups)
			   		 ; (display "\n")
			   		 ; (display "dups - min-dups: ")  (display rest-non-min-dups)
			   		 ; (display "\n")
			   		 ; (display "final: ")  (display final-dups)
			   		 ; (display "\n")
			   		 ; "bla"
			  
			   		
			 				
	)	))



