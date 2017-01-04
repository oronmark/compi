(include "compiler.scm")

;;helper functions

(define disp
  (lambda (exp)
    (display "\n")
    (display "The expression is: ")
    (display exp)
    (display "\n")))
    

    
(define get-or-body
	(lambda (or-expr)
		(cadr or-expr)
		))

(define remove-last-elem
	(lambda (lst)
		(if (null? (cdr lst))
			'()
			(cons (car lst) (remove-last-elem (cdr lst))))
		))

(define get-last-elem
	(lambda (lst)
		(if (null? (cdr lst))
                    (car lst)
	            (get-last-elem (cdr lst)))
		))

(define get-if-test
  (lambda (if-exp)
    (cadr if-exp)))

(define get-if-dit
  (lambda (if-exp)
    (caddr if-exp)))

(define get-if-dif
  (lambda (if-exp)
    (cadddr if-exp)))

(define get-def-var
  (lambda (def-expr)
    (cadr def-expr)))

(define get-def-val
  (lambda (def-expr)
    (caddr def-expr)))

(define get-set-var
  (lambda (set-expr)
    (cadr set-expr)))

(define get-set-val
  (lambda (set-expr)
    (caddr set-expr)))


(define get-lambda-opt-param
  (lambda (l-expr)
    (cadr expr)))

(define get-lambda-opt-param-list
  (lambda (l-expr)
    (caddr expr)))

(define get-lambda-var-param
  (lambda (l-expr)
    (cadr l-expr)))

(define get-lambda-simple-param
  (lambda (l-expr)
    (cadr l-expr)))

(define get-lambda-simple-body
  (lambda (l-expr)
    (caddr l-expr)))

(define get-lambda-opt-body
  (lambda (l-expr)
    (cadddr l-expr)))

(define get-lambda-var-body
  (lambda (l-expr)
    (caddr l-expr)))

(define get-applic-operator
  (lambda (applic-expr)
    (cadr applic-expr)))

(define get-applic-operands
  (lambda (applic-expr)
    (caddr applic-expr)))

(define get-seq-list
	(lambda (seq-expr)
		(cadr seq-expr)
		))

(define get-lambda-body
  (lambda (l-expr)
    (cond  ((equal? (car l-expr) 'lambda-simple) (get-lambda-simple-body l-expr))
           ((equal? (car l-expr) 'lambda-opt) (get-lambda-opt-body l-expr))
           (else  (get-lambda-var-body l-expr)))))

(define is-lambda-expr?
  (lambda (expr)
    (or (equal? (car expr) 'lambda-simple) (equal? (car expr) 'lambda-opt) (equal? (car expr) 'lambda-var))))


(define is-lambda-seq?
  (lambda (l-expr)
     (cond  ((equal? (car l-expr) 'lambda-simple) (equal? (caaddr l-expr) 'seq))
            ((equal? (car l-expr) 'lambda-opt) (equal? (car (cadddr l-expr)) 'seq))
            (else  (equal? (caaddr l-expr) 'seq)))))


(define member-simple? 
	(lambda (l-expr v)
		(ormap (lambda (x) (equal? v x)) (get-lambda-simple-param l-expr))
		))

(define member-opt?
	(lambda (l-expr v)
		(or (ormap (lambda (x) (equal? v x)) (get-lambda-opt-param l-expr)) (equal? (get-lambda-opt-param-list l-expr) v))
		))


(define member-var?
	(lambda (l-expr v)
		(equal? (get-lambda-var-param l-expr) v)
		))

(define member-param?
	(lambda (l-expr v)
		(cond ((equal? (car l-expr ) 'lambda-simple) (member-simple? l-expr v))
          ((equal? (car l-expr ) 'lambda-opt) (member-opt? l-expr v))
          (else  (member-var? l-expr v)))
		))



;--------------------------------------------------------------------------------------;;part 4

(define make-expr-no-redundant
	(lambda (expr)
		(let*  
			((op (cadr expr))
			 (args (caddr expr)))	
				(if (and (is-lambda-expr? expr) (null? args))
                                    (get-lambda-body expr)
                                    expr)
			)
 ))


					
(define remove-applic-lambda-nil
 	(lambda (expr)
 		(cond ((atom? expr) expr)
 	              ((equal? (car expr) 'applic) (map remove-applic-lambda-nil (make-expr-no-redundant expr)))
 	              (else (map remove-applic-lambda-nil expr))
 			)
 		))



;;--------------------------------------------------------------------------------------- part 7

;;need to fix using part 6  
(define tc
        (lambda (expr tp?)
         ;  (disp expr)
		(cond ((equal? (car expr) 'or) `(or ,(append (map (lambda (ex) (tc ex #f)) (remove-last-elem (get-or-body expr))) (list (tc (get-last-elem (get-or-body expr)) tp?)))))
                 ((equal? (car expr) 'seq) `(seq ,(append (map (lambda (ex) (tc ex #f)) (remove-last-elem (get-or-body expr))) 
                                                          (list (tc (get-last-elem (get-or-body expr)) tp?)))))
                 ((equal? (car expr) 'set) `(set ,(get-set-var expr) ,(tc (get-set-val expr) tp?)))
                 ((equal? (car expr) 'if3) `(if3 ,(tc (get-if-test expr) #f) ,(tc (get-if-dit expr) tp?) ,(tc (get-if-dif expr) tp?)))
                 ((equal? (car expr) 'def) `(def ,(get-def-var expr) ,(tc (get-def-val expr) #f)))
                 ((equal? (car expr) 'lambda-simple) `(lambda-simple ,(get-lambda-simple-param expr) ,(tc (get-lambda-simple-body expr) #t)))
                 ((equal? (car expr) 'lambda-opt) `(lambda-opt ,(get-lambda-opt-param expr) ,(get-lambda-opt-param-list expr)  ,(tc (get-lambda-opt-body expr) #t)))
                 ((equal? (car expr) 'lambda-var) `(lambda-var ,(get-lambda-var-param expr),(tc (get-lambda-var-body expr) #t)))
                 ((equal? (car expr) 'applic) (if (eq? tp? #f)
                          `(applic ,(tc (get-applic-operator expr) #f) ,(map (lambda (ex) (tc ex #f)) (get-applic-operands expr)))
                          `(tc-applic ,(tc (get-applic-operator expr) #f) ,(map (lambda (ex) (tc ex #f)) (get-applic-operands expr)))))
                 (else expr))
		))

(define annotate-tc
  (lambda (expr)
  	(tc expr #f)))

;;--------------------------------------------------------------------------------------- part 6

(define seq?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'seq))))

(define seq-exps
  (lambda(exp)
    (cadr exp)))

(define lambda-simple-pred
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-simple))))

(define lambda-simple-vars
  (lambda(exp)
    (cadr exp)))

(define lambda-simple-body
  (lambda(exp)
    (caddr exp)))

(define lambda-opt-pred
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-opt))))

(define lambda-opt-vars
  (lambda(exp)
    (cadr exp)))

(define lambda-opt-last
  (lambda(exp)
    (caddr exp)))

(define lambda-opt-body
  (lambda(exp)
    (cadddr exp)))

(define lambda-variadic-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-var))))

(define lambda-var-vars
  (lambda(exp)
    (cadr exp)))

(define lambda-var-body
  (lambda(exp)
    (caddr exp)))

(define my-var?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'var))))

(define get-my-var?
  (lambda(exp)
    (cadr exp)))

(define get-position
  (lambda(item  stack  )
    (letrec ((iter 
              (lambda(stack item i)
                (if (equal? (car stack) item) i
                    (iter (cdr stack) item (+ i 1))))))
      (iter stack item 0))))

(define member?
  (lambda(item env)
    (cond ((null? env) #f)
          ((member item (car env)) #t)
          (else (member? item (cdr env) )))))

(define item-pos-env
  (lambda(item env)
    (letrec ((iter
              (lambda(env item i)
                (if (member item (car env)) 
                    (cons (car env) i) 
                    (iter (cdr env) item (+ i 1)  )))))
      (let* ((stack-row (iter env item 0))
             (stack (car stack-row))
             (row (cdr stack-row))
             (col (get-position item stack)))
        (cons row col)))))

(define pe->lex-pe
  (lambda (expr)
        (annotate-helper expr '() '())
    ))

(define annotate-helper 
  (lambda (exp stack env)
    (cond ((lambda-simple-pred exp)
           (let* ((inner-stack (lambda-simple-vars exp))
                  (body (lambda-simple-body exp))
                  (inner-env  (cons stack env)))
             (if (seq? body) 
                 `(lambda-simple ,inner-stack (seq ,(map (lambda(exp) (annotate-helper  exp inner-stack inner-env)) (seq-exps body))))  
                 `(lambda-simple ,inner-stack ,(annotate-helper body inner-stack inner-env)))))
          ((lambda-variadic-exp? exp)
           (let* ((var  (lambda-var-vars exp))
                  (inner-stack (list var))
                  (body (lambda-var-body exp))
                  (inner-env  (cons stack env)))         
             (if (seq? body) 
                 `(lambda-var ,var (seq ,(map (lambda(exp) (annotate-helper  exp inner-stack inner-env)) (seq-exps body))))  
                 `(lambda-var ,var ,(annotate-helper body inner-stack inner-env)))))
          ((lambda-opt-pred exp)
           (let* ((vars (lambda-opt-vars exp))
                  (last-var (lambda-opt-last exp))
                  (inner-stack (append vars (list last-var)))
                  (body (lambda-opt-body exp))
                  (inner-env  (cons stack env)))         
             (if (seq? body) 
                 `(lambda-opt ,vars ,last-var (seq ,(map (lambda(exp) (annotate-helper  exp inner-stack inner-env)) (seq-exps body))))  
                 `(lambda-opt ,vars ,last-var ,(annotate-helper body inner-stack inner-env)))))
          ((my-var? exp)
           (let ((var (get-my-var? exp)))
             (cond ((member var stack) `(pvar ,var ,(get-position var  stack  )  ))
                   ((member? var env)
                    (let ((pos (item-pos-env var env)))
                      `(bvar ,var ,(car pos) ,(cdr pos))))
                   (else `(fvar ,var)))))
          ((list? exp) (map (lambda(exp) (annotate-helper  exp stack env)) exp))
          (else exp)
          )))

; ------------------------------------- Eliminate Nested Defines ;

(define eliminate-nested-defines2
  (lambda (expr)
    expr))

(define eliminate-nested-defines
  (lambda (expr)
      (nested-def-parser expr) 
    ))

(define zip
  (lambda (l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (cons (list (car l1) (car l2))
              (zip (cdr l1) (cdr l2))))))

(define nested-seq-helper
  (lambda (vars vals)
    (map (lambda (pair) `(set ,(car pair) ,(cadr pair))) (zip vars vals))
    ))

(define build-applic-nested-def
  (lambda (ribs rest)
        (let* ((vars (map (lambda (x) (cadr x)) ribs))
              (vals (map (lambda (x) (nested-def-parser (caddr x))) ribs))
              (args (map (lambda (x) (cadr x)) vars))
              (applics (map (lambda (x) '(const #f)) ribs)))
          `(applic (lambda-simple ,args (seq (,@(nested-seq-helper vars vals) ,(nested-def-parser (car rest))))) ,applics)
          )))

(define contain-defs?
  (lambda(expr)
    (ormap (lambda(exp) (def-expr? exp)) expr)
    ))

(define def-expr?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'def))
    ))

(define break-nested
  (lambda (expr)
    (let  ((listOfdefines (mayer-break expr (lambda (x y) x))) ;get define
           (rest (mayer-break expr (lambda(x y) y)))) ; get rest of body
           (build-applic-nested-def listOfdefines rest)
    )))

(define mayer-break
  (lambda (pes ret-ds-es)
    (if (null? pes) (ret-ds-es '() '())
      (mayer-break
        (cdr pes)
        (lambda (ds es)
          (cond ((eq? (caar pes) 'def)
                 (ret-ds-es (cons (car pes) ds) es))
                ((eq? (caar pes) 'seq)
                 (mayer-break (cadar pes)
                        (lambda (ds1 es1)
                          (ret-ds-es (append ds1 ds)
                                     (append es1 es)))))
                (else (ret-ds-es ds (cons (car pes) es)))))))))

(define nested-def-parser
  (let ((run
          (compose-patterns
            (pattern-rule
              `(lambda-simple ,(? 'vars) ,(? 'body) . ,(? 'bodies))
               (lambda (vars body bodies)
                 `(lambda-simple ,vars ,(nested-def-parser body))
                 ))
            (pattern-rule
              `(lambda-var ,(? 'vars) ,(? 'body) . ,(? 'bodies))
               (lambda (vars body bodies)
                 `(lambda-var ,vars ,(nested-def-parser body))
                 ))
            (pattern-rule
              `(lambda-opt ,(? 'vars) ,(? 'body) . ,(? 'bodies))
               (lambda (vars body bodies)
                 ;bodies
                 `(lambda-opt ,vars ,(nested-def-parser body) ,@(nested-def-parser bodies))
                 ))
            (pattern-rule
              `(applic ,(? 'lambda) . ,(? 'args))
               (lambda (lambda-expr args)
                 `(applic ,(nested-def-parser lambda-expr) ,@(nested-def-parser args))
                 ))
            (pattern-rule
              `(seq ,(? 'body))
                (lambda (body)
                  (if (contain-defs? body)
                   (break-nested body)
                  `(seq ,body))
                  ))
            (pattern-rule
              `(def ,(? 'var)  ,(? 'body))
                (lambda (var body)
                 `(def ,var ,(nested-def-parser body))
                 ))
            )))
        (lambda (sexpr)
          (run sexpr (lambda() sexpr)))))


; ------------------------------------- Eliminate Nested Defines ;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













