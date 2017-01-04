
 
(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)
 
;Signature: with(s f)
;Type: [List*Proc -> Val]
;Purpose:apply proc on args
(define with
  (lambda (s f) 
    (apply f s))) 

;Signature: match(pat e ret-with ret-fail) mair's code from "pattern-matcher.scm"
(define match
  (letrec ((match
	    (lambda (pat e ret-vals ret-fail)
	      (cond ((and (pair? pat) (pair? e))
		     (match (car pat) (car e)
			    (lambda (vals-car)
			      (match (cdr pat) (cdr e)
				     (lambda (vals-cdr)
				       (ret-vals
					(append vals-car vals-cdr)))
				     ret-fail))
			    ret-fail))
		    ((and (vector? pat) (vector? e)
			  (= (vector-length pat) (vector-length e))
			  (match (vector->list pat) (vector->list e)
				 ret-vals ret-fail)))
		    ((procedure? pat)
		     (let ((v (pat e)))
		       (if v (ret-vals v) (ret-fail))))
		    ((equal? pat e) (ret-vals '()))
		    (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
	     (lambda (vals) (apply ret-with vals))
	     ret-fail))))
;Signature: ?(name . guards) mair's code from "pattern-matcher.scm"
(define ?
  (lambda (name . guards)
    (let ((guard?
	   (lambda (e)
	     (andmap 
	      (lambda (g?) (g? e))
	      guards))))
      (lambda (value)
	(if (guard? value)
	    (list value)
	    #f)))))

(define _ (? 'dont-care))

;;; composing patterns
;Signature: pattern-rule (pat handler) mair's code from "pattern-matcher.scm"
(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

;compose-patterns (pat handler) mair's code from "pattern-matcher.scm"
(define compose-patterns
  (letrec ((match-nothing
	    (lambda (e failure)
	      (failure)))
	   (loop
	    (lambda (s)
	      (if (null? s)
		  match-nothing
		  (let ((match-rest
			 (loop (cdr s)))
			(match-first (car s)))
		    (lambda (e failure)
		      (match-first e
		       (lambda ()
			 (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))

;Signature: expand-letrec(letrec-expr)
;Type: [letrec-exp -> let-exp]
;Purpose:transform a letrec expression to a let expression
(define expand-letrec
  (lambda (letrec-expr)
    (with letrec-expr
          (lambda (_letrec ribs . exprs)
            (let* ((fs (map car ribs))
                   (lambda-exprs (map cdr ribs))
                   (nu (gensym))
                   (nu+fs `(,nu ,@fs))
                   (body-f `(lambda ,nu+fs ,@exprs))
                   (hofs
                    (map (lambda (lambda-expr) `(lambda ,nu+fs ,@lambda-expr))
                         lambda-exprs)))
              `(Ym ,body-f ,@hofs))))))

;Signature: expand-qq(e)
;Type: [quasiquote-exp ->quasiquote_less-exp]
;Purpose:transform a quasiquoted expression  to an  equivalent expression with no quasiquote
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
          ((unquote-splicing? e)
           (error 'expand-qq "unquote-splicing here makes no sense!"))
          ((pair? e)
           (let ((a (car e))
                 (b (cdr e)))
             (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
                   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
                   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
          ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
          ((or (null? e) (symbol? e)) `',e)
          (else e))))

;Signature: ^quote?(tag)
;Type: symbol -> (exp->boolean)
;Purpose: returns a proc that checks if an expression is a tagged by tag 
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

;unquote?
(define unquote? (^quote? 'unquote))

;unquote-splicing?
(define unquote-splicing? (^quote? 'unquote-splicing))

;*void-object*
(define *void-object* (if #f #f )) 

;*reserved-words*
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote 
        unquote-splicing quote ))

;Signature: reserved?(word)
;Type: symbol -> boolean
;Purpose:check is word is a reserved-word
(define reserved?
  (lambda(word)
    (member word *reserved-words*)))

;Signature: simple-const?(x)
;Type: exp->boolean
;Purpose:check exp is a const
(define simple-const?
  (lambda(x)
    (or (number? x) (boolean? x) (char? x) (string? x))))

;Signature: var?(x)
;Type: exp->boolean
;Purpose:check exp is a variable
(define var?
  (lambda(x)
    (and (symbol? x) (not (reserved? x)))))

;Signature: mybeginify(expr-list)
;Type: List->List
;Purpose:creates a sequance of parsed arguments
(define mybeginify
  (lambda(expr-list)
    (letrec (( beginify  (lambda(expr-list)
                           (if (null? expr-list) 
                               (list)
                               (cons (parse (car expr-list)) (beginify (cdr expr-list)))))))
      (cond ((= (length expr-list) 1)  (parse (car expr-list)))
            (else `(seq ,(beginify expr-list)))
            ))))

;Signature: begin-args(expr-list)
;Type: List->List
;Purpose:creates a list of parsed arguments
(define begin-args
  (lambda(expr-list)
    (if (null? expr-list) 
        (list)
        (cons (parse (car expr-list)) (begin-args (cdr expr-list))))))

;Signature: opt-lambda-vars?(vars)
;Type: List->boolean
;Purpose:checs if vars are proper lambda-opt vars
(define opt-lambda-vars?
  (lambda(vars)
    (if (pair? vars)
        (and (var? (car vars)) (opt-lambda-vars? (cdr vars)))
        (var? vars))))

;Signature: get-opt-lambda-vars(vars)
;Type: List->List
;Purpose:gets lambda-opt vars
(define get-opt-lambda-vars
  (lambda(vars)
    (if (pair? vars)
        (cons (car vars) (get-opt-lambda-vars (cdr vars)))
        (list))))

;Signature: get-opt-lambda-vars-rest(vars)
;Type: List->List
;Purpose:gets lambda-opt vars from the second
(define get-opt-lambda-vars-rest
  (lambda(vars)
    (if (pair? vars)
        (get-opt-lambda-vars-rest (cdr vars))
        vars)))

;Signature: hasDuplicates?(vars)
;Type: List->boolean
;Purpose:check if this list has duplicate items
(define hasDuplicates?
  (lambda(vars)
    (cond ((null? vars) #f)
          ((member (car vars) (cdr vars)) #t)
          (else (hasDuplicates? (cdr vars))))))

;Signature: proper-let-vars?(vars)
;Type: let-exp-vars->boolean
;Purpose:check if  vars are proper proper ler vars
(define proper-let-vars?
  (lambda(ob)
    (and
     (andmap list? ob) 
     (andmap pair? ob) 
     (andmap (lambda(item) (var? (car item))) ob )
     (not (hasDuplicates? (map car ob))))))

;Signature: parse(e)
;Type: exp->parsed-exp
;Purpose:parses the expression
(define parse
  (let ((run
         (compose-patterns  
          ;quasiquote 
          (pattern-rule  
           `(,'quasiquote  ., (? 'exp));add checks / tests
           (lambda (exp) ; 
             (if (= (length exp) 1)
                 (parse (expand-qq (car exp)))
                 (error "quasiquote : " "1 expression expected"))))  
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c)  `(const ,c)))
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          
          ;if
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
          ;simple-lambda
          (pattern-rule
           `(lambda ,(? 'lambda-vars list? (lambda(vars) (andmap var? vars))) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr rest-body)
             (let ((body (cons expr rest-body)))
               `(lambda-simple ,vars ,(parse `(begin ,@body))))))
          ;opt-lambda
          (pattern-rule
           `(lambda ,(? 'lambda-vars  pair?  opt-lambda-vars?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars  expr rest-body)
             (let ((body (cons expr rest-body)))
               `(lambda-opt ,(get-opt-lambda-vars vars) ,(get-opt-lambda-vars-rest vars) ,(parse `(begin ,@body))))))
          ;variadic-lambda
          (pattern-rule
           `(lambda ,(? 'var var?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var  expr rest-body)
             (let ((body (cons expr rest-body)))
               `(lambda-variadic ,var ,(parse `(begin ,@body))))))
          ;begin
          (pattern-rule
           `(begin  .,(? 'exprs list?))
           (lambda (exprs)
             (cond  ((null? exprs) `(const ,*void-object*))
                    (else (mybeginify exprs))))) 
          ;define 
          (pattern-rule
           `(define ,(? 'var var?)  ,(? 'expr))
           (lambda (var expr)
             `(define ,(parse var) ,(parse expr))))
          ;mit define
          (pattern-rule
           `(define ,(? 'vars)  ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr rest-body)
             `(define ,(parse (car vars))  ,(parse `(lambda ,(cdr vars) ,expr ,@rest-body )))))
          
          ;applic
          (pattern-rule
           `( ,(? 'proc  (lambda(pr)(not (reserved? pr)))) . ,(? 'args list?))
           (lambda (proc args)
             `(applic ,(parse proc)  ,(begin-args args))))
          ;and
          (pattern-rule
           `(and . ,(? 'args list?))
           (lambda (args)
             (cond ((null? args) (parse #t))
                   ((= (length args) 1) (parse (car args)))
                   (else (parse `(if  ,(car args)    (and ,@(cdr args))  #f))))))
      
          ;cond
          (pattern-rule
           `(cond (,(? 'pred) ,(?'exp ) .,(? 'rest-exps))  . ,(? 'rest))
           (lambda (pred exp rest-exps rest )
             (cond ((and (null? rest) (not (equal? pred 'else))) 
                    (parse `(if ,pred (begin ,@(cons exp rest-exps))))) ; no else present
                   ((and (null? rest) (equal? pred 'else))
                    (parse `(begin ,@(cons exp rest-exps))))
                   (else (parse `(if ,pred  (begin ,@(cons exp rest-exps)) (cond ,@rest)))))))
          
          ;let  
          (pattern-rule
           `(let ,(? 'vars-exps proper-let-vars?)  ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr rest-body)
             (let ((body (cons expr rest-body))
                   (let-vars (map car vars) )
                   (let-exps (map cadr vars)))
               (parse `((lambda ,let-vars ,@body) ,@let-exps)))))
          ;letrec 
          (pattern-rule
           `(letrec ,(? 'vars-exps proper-let-vars?)  ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr rest-body)
                (parse (expand-letrec `(letrec ,vars ,expr ,@rest-body)))))
          ;; let* 
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (let ((body (cons expr exprs)))
               (parse `(begin ,@body)))))
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
           (lambda (var val rest exprs)
             (parse `(let ((,var ,val))
                       (let* ,rest . ,exprs)))))
           ;or 
           (pattern-rule
           `(or . ,(? 'args list?))
           (lambda (args) 
             (cond ((null? args) (parse #f))
                   ((= (length args) 1) (parse (car args)))
                   (else  (let ((mapped-args (map parse args)))
                                 `(or ,mapped-args))
                             )))) 
          )))
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "I can't recognize this: ~s" e)))))))
 
 
;Signature:seq?(exp)
;Type: exp->boolean
;Purpose:check if exp is a sequence
(define seq?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'seq))))
;Signature:get-seq-exps(exp)
;Type:seq->seqquence expressions
;Purpose:get the expressions of a sequence
(define get-seq-exps
  (lambda(exp)
    (cadr exp)))
;Signature:lambda-simple-exp?(exp)
;Type: exp->boolean
;Purpose:check if exp is a simple-lambda
(define lambda-simple-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-simple))))
;Signature:get-seq-exps(exp)
;Type:simple-lambda exp->simple-lambd variables
;Purpose:get the variables of a simple-lambda
(define get-lambda-simple-vars
  (lambda(exp)
    (cadr exp)))
;Signature:get-lambda-simple-body(exp)
;Type:simple-lambda exp->simple-lambd body
;Purpose:get the body of a simple-lambda
(define get-lambda-simple-body
  (lambda(exp)
    (caddr exp)))
;Signature:lambda-opt-exp?(exp)
;Type:exp->boolean
;Purpose:check if exp is an opt-lambda expression
(define lambda-opt-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-opt))))
;Signature:get-lambda-opt-vars(exp)
;Type:opt-lambda expression-> opt-lambda variables
;Purpose:gets the variables of an opt-lambda
(define get-lambda-opt-vars
  (lambda(exp)
    (cadr exp)))
;Signature:get-lambda-opt-extra-var(exp)
;Type:opt-lambda expression-> opt-lambda extra variables
;Purpose:gets the extra variable of an opt-lambda
(define get-lambda-opt-extra-var
  (lambda(exp)
    (caddr exp)))
;Signatureget-lambda-opt-body(exp)
;Type:opt-lambda expression-> opt-lambda body
;Purpose:gets the body of an opt-lambda
(define get-lambda-opt-body
  (lambda(exp)
    (cadddr exp)))
;Signature:lambda-variadic-exp?(exp)
;Type:exp->boolean
;Purpose:check if exp is an variadic-lambda expression
(define lambda-variadic-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-variadic))))
;Signature:get-lambda-variadic-var(exp)
;Type:variadic-lambda expression-> variadic-lambda variables
;Purpose:gets the variables of a variadic-lambda
(define get-lambda-variadic-var
  (lambda(exp)
    (cadr exp)))
;Signature:get-lambda-variadic-body(exp)
;Type:variadic-lambda expression-> variadic-lambda body
;Purpose:gets the body of a variadic-lambda
(define get-lambda-variadic-body
  (lambda(exp)
    (caddr exp)))
;Signature:my-var?(exp)
;Type:exp->boolean
;Purpose:check if exp is a variable
(define my-var?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'var))))
;Signature:get-my-var?(exp)
;Type:var expression->variable name
;Purpose:gets the variable's name
(define get-my-var?
  (lambda(exp)
    (cadr exp)))
;Signature:get-item-position-in-stack(item  stack  )
;Type:item*stack->int
;Purpose:gets the position of item on the stack
(define get-item-position-in-stack
  (lambda(item  stack  )
    (letrec ((iter 
              (lambda(stack item i)
                (if (equal? (car stack) item) i
                    (iter (cdr stack) item (+ i 1))))))
      (iter stack item 0))))
;Signature:member-in-env(item env)
;Type:item*env->boolean
;Purpose:check if item is a memeber of env(a stack of stacks)
(define member-in-env
  (lambda(item env)
    (cond ((null? env) #f)
          ((member item (car env)) #t)
          (else (member-in-env item (cdr env) )))))
;Signature:get-item-position-in-env(item env)
;Type:item*env->int
;Purpose:gets the position of item on the environment
(define get-item-position-in-env
  (lambda(item env)
    (letrec ((iter
              (lambda(env item i)
                (if (member item (car env)) 
                    (cons (car env) i) 
                    (iter (cdr env) item (+ i 1)  )))))
      (let* ((stack-row (iter env item 0))
             (stack (car stack-row))
             (row (cdr stack-row))
             (col (get-item-position-in-stack item stack)))
        (cons row col)))))

;Signature:pe->lex-pe(pe)
;Type:parsed-expression->lexical-parsed-expression
;Purpose:turns all variables of pe to their analogous lexical variables
(define pe->lex-pe
  (lambda(pe)
    (letrec ((annotate-scope
              (lambda(exp stack env)
    (cond ((lambda-simple-exp? exp)
           (let* ((new-stack (get-lambda-simple-vars exp))
                  (body-exp (get-lambda-simple-body exp))
                  (new-env  (cons stack env)))         
             (if (seq? body-exp) 
                 `(lambda-simple ,new-stack (seq ,(map (lambda(exp) (annotate-scope  exp new-stack new-env)) (get-seq-exps body-exp))))  
                 `(lambda-simple ,new-stack ,(annotate-scope body-exp new-stack new-env)))))
          ((lambda-opt-exp? exp)
           (let* ((vars (get-lambda-opt-vars exp))
                  (extra-var (get-lambda-opt-extra-var exp))
                  (new-stack (append vars (list extra-var)))
                  (body-exp (get-lambda-opt-body exp))
                  (new-env  (cons stack env)))         
             (if (seq? body-exp) 
                 `(lambda-opt ,vars ,extra-var (seq ,(map (lambda(exp) (annotate-scope  exp new-stack new-env)) (get-seq-exps body-exp))))  
                 `(lambda-opt ,vars ,extra-var ,(annotate-scope body-exp new-stack new-env)))))
          ((lambda-variadic-exp? exp)
           (let* ((var  (get-lambda-variadic-var exp))
                  (new-stack (list var))
                  (body-exp (get-lambda-variadic-body exp))
                  (new-env  (cons stack env)))         
             (if (seq? body-exp) 
                 `(lambda-variadic ,var (seq ,(map (lambda(exp) (annotate-scope  exp new-stack new-env)) (get-seq-exps body-exp))))  
                 `(lambda-variadic ,var ,(annotate-scope body-exp new-stack new-env)))))
          ((my-var? exp)
           (let ((var (get-my-var? exp)))
             (cond ((member var stack) `(pvar ,var ,(get-item-position-in-stack var  stack  )  ))
                   ((member-in-env var env)
                    (let ((pos (get-item-position-in-env var env)))
                      `(bvar ,var ,(car pos) ,(cdr pos))))
                   (else `(fvar ,var)))))
          ((list? exp) (map (lambda(exp) (annotate-scope  exp stack env)) exp))
          (else exp)))))
    (annotate-scope pe '() '())) ))

;Signature:or-exp(exp)
;Type:exp->boolean
;Purpose:check if exp is an or expression
(define or-exp
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'or))))
;Signature:get-or-body(exp)
;Type:exp-> or-body
;Purpose:get's the expressions of an or expression
(define get-or-body
  (lambda(exp)
    (cadr exp)))
;Signature:get-all-but-last(ll)
;Type:List-> List
;Purpose:removes the last object on List ll
(define get-all-but-last
  (lambda(ll)
    (if (null? (cdr ll)) (list)
        (cons (car ll) (get-all-but-last (cdr ll))))))
;Signature:get-last(ll)
;Type:List-> Object
;Purpose:gets last object on List
(define get-last
  (lambda(ll)
    (if (null? (cdr ll)) (car ll)
         (get-last (cdr ll)))))
;Signature:if-exp?(exp)
;Type:exp->boolean
;Purpose:check if exp is an if expression
(define if-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'if3))))
;Signature:get-if-test(exp)
;Type:if-exp->if-test
;Purpose:gets the test expression from an if expression
(define get-if-test
  (lambda(exp)
    (cadr exp)))
;Signature:get-if-dit(exp)
;Type:if-exp->if-dit
;Purpose:gets the dit expression from an if expression
(define get-if-dit
  (lambda(exp)
    (caddr exp)))
;Signature:get-if-dif(exp)
;Type:if-exp->if-dif
;Purpose:gets the dif expression from an if expression
(define get-if-dif
  (lambda(exp)
    (cadddr exp)))
;Signature:define-exp?(exp)
;Type:exp->boolean
;Purpose:check if exp is a define expression
(define define-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'define))))
;Signature:get-define-var(exp)
;Type:define-exp->define-var
;Purpose:gets the var  from a define expression
(define get-define-var
  (lambda(exp)
    (cadr exp)))
;Signature:get-define-exp(exp)
;Type:define-exp->define-value
;Purpose:gets the value  from a define expression
(define get-define-exp
  (lambda(exp)
    (caddr exp)))
;Signature:applic-exp?(exp)
;Type:exp->boolean
;Purpose:check if exp is an applic expression
(define applic-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'applic))))
;Signature:get-applic-proc(exp)
;Type:applic-exp->applic-proc
;Purpose:gets the procedure from an applic expression
(define get-applic-proc
  (lambda(exp)
    (cadr exp)))
;Signature:get-applic-args(exp)
;Type:applic-exp->applic-args
;Purpose:gets the arguments from an applic expression
(define get-applic-args
  (lambda(exp)
    (caddr exp)))

;Signature:annotate-tc(lex-pe)
;Type:lexical-parsed-expression->lexical-parsed-expression-tail-optimized
;Purpose:turns lex-pe to a tail optimized expression
(define annotate-tc
  (lambda(lex-pe)
    (letrec ((atc
              (lambda(exp tc?)
                (cond ((or-exp exp)  ;or contains at least 2 bodies to eval
                       (let ((body (get-or-body exp)))
                         `(or ,(append (map (lambda(e) (atc e #f)) (get-all-but-last body)) (list (atc (get-last body) tc?))))))
                      ((if-exp? exp) `(if3 ,(atc (get-if-test exp) #f) ,(atc (get-if-dit exp) tc?) ,(atc (get-if-dif exp) tc?)))
                      ((seq? exp) 
                       (let ((body (get-seq-exps exp)))
                         `(seq ,(append (map (lambda(e) (atc e #f)) (get-all-but-last body)) (list (atc (get-last body) tc?))))))
                      ((lambda-simple-exp? exp)
                       (let ((body (get-lambda-simple-body exp))
                             (vars (get-lambda-simple-vars exp)))
                         `(lambda-simple ,vars ,(atc body #t))))
                      ((lambda-opt-exp? exp)
                       (let ((body (get-lambda-opt-body exp))
                             (vars (get-lambda-opt-vars exp))
                             (extra-var (get-lambda-opt-extra-var exp)))
                         `(lambda-opt ,vars ,extra-var ,(atc body #t))))
                      ((lambda-variadic-exp? exp)
                       (let* ((var  (get-lambda-variadic-var exp))
                              (body (get-lambda-variadic-body exp)))           
                         `(lambda-variadic ,var ,(atc body #t))))
                      ((define-exp? exp)
                       (let* ((var  (get-define-var exp))
                              (body (get-define-exp exp)))           
                         `(define ,var ,(atc body #f))))
                      ((applic-exp? exp)
                       (let* ((proc  (get-applic-proc exp))
                              (args (get-applic-args exp)))
                         (if tc?
                         `(tc-applic ,(atc proc #f) ,(map (lambda(e) (atc e #f)) args))
                         `(applic ,(atc proc #f) ,(map (lambda(e) (atc e #f)) args)))))
                      (else exp)))))
      (atc lex-pe #f))))
                      