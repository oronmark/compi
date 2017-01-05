;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - ASS3 - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "box.scm")
(define test-func (lambda (x) 
		    (annotate-tc
		      (pe->lex-pe
			(box-set
			  (remove-applic-lambda-nil
			    (eliminate-nested-defines (parse x))))))))
			    
(load "hw3.so")			    

(define tests-counter 0)
(define failed-tests-counter 0)

;;;; Configuration
(define show-passed-tests #f)
(define show-summary #t)

(define show-difference
  (lambda (actual expected)
    (if (or (null? actual) (null? expected)) ""
      (if (equal? (car actual) (car expected))
	  (begin (display (format "\033[1;32m~s\033[0m" (car actual)))
			(show-difference (cdr actual) (cdr expected)))
	  (begin (display (format "\033[1;31m~s\033[0m" (car actual)))
			(show-difference (cdr actual) (cdr expected)))))
))	
			    
(define assert
	(lambda (input)
		(set! tests-counter (+ 1 tests-counter))
		(let ((actual-output (test-func input))
		      (expected-output (full-cycle input)))
			(cond ((equal? actual-output expected-output)
				(if show-passed-tests
				  (begin (display (format "~s) ~s\n" tests-counter input))
				  (display (format "\033[1;32m Success! ☺ \033[0m \n\n")))) 
				  #t)
				(else
				  (set! failed-tests-counter (+ 1 failed-tests-counter))
				  (display (format "~s) ~s\n" tests-counter input))
				  (display (format "\033[1;31mFailed! ☹\033[0m\n\n\033[1;34mExpected:\n ~s\033[0m\n\n\033[1;29mActual:\n ~s\033[0m\n\n" expected-output actual-output))
				#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map assert lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
	(if show-summary
	  (begin
	    (display (format "Summary\n=============================\n\033[1;32mPassed: ~s of ~s tests ☺\033[0m\n" (- tests-counter failed-tests-counter) tests-counter))
	    (if (> failed-tests-counter 0)
	      (display (format "\033[1;31mFailed: ~s of ~s tests ☹\033[0m\n\n" failed-tests-counter tests-counter)))))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m!!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m#####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define Tests
  (list
    '(lambda (x) x)
    
    '(lambda (x y) x y (lambda (a b c) a b c))
    
    '(lambda (x y) (lambda (a b c) a b c) x y)
				    
    '(lambda (x y) ((lambda (a b c) a b c) 5 6 7) x y)
				   
    '(lambda (a b c d e) ((lambda (a b c) (lambda (z e1) (e1 5))) 5 6 7))
							  
    '(lambda (a b c d e) ((lambda (a b c) (lambda () 5))))

    '(lambda (x) (define a 5) b)
	    
    '(lambda (x) (lambda (y) (define x 10) (a 5)))

    '(lambda (x) (define y (lambda () (define a 5) 4)) 1)
      
    '(lambda (x) (define a 5) (lambda (y) (define x 10) (a 5)))
    '(lambda (x) (define a 5) (define a1 123) (lambda (y) (define x 10) (a 5)) 2)
    '(lambda (x) (define a 5) (lambda () (define a 123) (a #t)) (lambda (y) (define x 10) (a 5)) 2)
     
      '(lambda (z) (define a 5) (define b 123) (lambda (y) (define x 10) 
	(define x1 (lambda (abc) (define a 56) (define x1 10) (+ 1 2))) (f 32 45 'a)) (a 5))

     
     '(define x (lambda (x) x))

        
     '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) #t))


      '(define odd? (lambda (n) (and (positive? n) (even? (- n 1)))))


      '(even? e)
      
      '(lambda x (lambda (a . b) a (lambda (b) b c (f (lambda (y) (define a 5) a)))))



     '(lambda (x) ((f (lambda (z) z)) (lambda (y) y)))


      '(lambda (x) (lambda (y) (lambda (z) z)))


      '(lambda (x) (lambda (y) (lambda (z) (define x 1) (define y 2) (define z 3) y)))

       
      '(lambda (x . y) 1)

	 
      '(lambda (x . y) (lambda (x) y x (set! x 1) (* x 1)))


      '(lambda (x) x (f (lambda (y) (define a 5) y)))

			  
    '(let ((a 0))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))))	

	  
    '(let* ((c 0)
	  (a (box c)))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))))


    '(lambda (a . b) a)

       
    '(lambda (a . b) (lambda () a))

       
    '(lambda (a . b) (begin (lambda () a) (set! a 5)))
    
    '1
    
    '(define mileage
      (lambda (thunk)
	(let ((loop ((eng (make-engine thunk)) (total-ticks 0))))
	  (eng 50
	    (lambda (ticks value)
	      (+ total-ticks (- 50 ticks)))
	    (lambda (new-eng)
	      (loop new-eng (+ total-ticks 50))))))) 
	      
    '(define round-robin
      (lambda (engs)
	(if (null? engs)
	    '()
	    ((car engs) 1
	      (lambda (ticks value)
		(cons value (round-robin (cdr engs))))
	      (lambda (eng)
		(round-robin
		  (append (cdr engs) (list eng))))))))			

		
    '(let ((a 0) (c 1))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))
	  (lambda () c)
	  (lambda () (set! c (+ a 1)))
	  (lambda (b) (set! c b))    
	))

	  
    '(lambda (a b) (begin a (set! a b) (lambda () a) (lambda (a b) (+ a b))))

		  
    '(lambda (a) a)

       
    '(lambda (a b) a c (lambda (a . b) b))


    '(lambda (a b) a (lambda (b . c) b))


    '(lambda () (+ 1 2))


    '(lambda (+) (+ 1 2))


    '(lambda a a)

       
    '(a (lambda a a) (lambda (a) a) a (lambda (a) (lambda () a) a))


    '((lambda (a) a) (lambda (b) b d) (lambda (c) c))


    '((lambda (a) a) (lambda (b) b) (lambda (c) c) d)

	    
    '(lambda (a b) (lambda (c) a))

       
    '(lambda (a b) (lambda (c) a (lambda () b)))


    '(lambda () a (lambda () (lambda () b)))

    
    '(f (g (h (a (b (abc (lambda () a (lambda () (lambda () b)))))))))


    '(lambda (a b) (lambda (c) a (lambda () b (lambda (d) c))))


    '(f (lambda (a b) (lambda (c) a (lambda () (lambda (d) (lambda x a))))))


    '((lambda (a) (begin a (lambda () a) (lambda (a) a))))


    '(lambda (a b) c d (lambda () c d))


    '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

		      
    '(lambda (a b) (lambda (c) (+ a b c)))


    '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) 


   '(set! x (f 1))

      
    '(or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

		  
    '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))		    

			
    '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))) (a (b 1)))


    '(and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

	
    '(let ((a 1)) (and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))	

	  
    '(begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

               
    '(((lambda (a b c d) (begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) ((lambda () ((lambda () d)))))))))))


   '((lambda () 5))

      
   '(lambda (x) (x x) (display "asaf"))


   '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))

		    
    '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))		    

	
    '(lambda (x) (x x))

	
   '((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))

      
   '(((lambda () f)) ((lambda () g)) ((lambda () h)) (z (m c (d ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))))
   
   '(lambda(x) (lambda (y) (set! x 1) (lambda () x)))
   
   '(or 3 4 (lambda (x) (define x 3) 5))
   
   '(lambda (a) a (lambda (b c) (set! a 4) (+ a b) (lambda () (set! b 8))))
   
   '(lambda (x) (lambda () (set! x 1)))
   
   '(lambda x (lambda () (set! x 1)))
   
   '(lambda (x . y) (lambda () (set! x 1)))
   
   '(or 1 (lambda (x) (x x)) (lambda (y) (y y)))
   
   '(lambda (a) (lambda () (set! a 3) b)) 
   
   '(lambda (a) (set! a 3) a)   
   
   '(a (lambda (a) (a (lambda (b) (a b (lambda (a) (a b (lambda (c) (a b c)))))))))
   
   '(define x (+ 1 (* 3 4) (- 5 6)))
   
   '(* (* 1 2) 3 (+ 4 5) ((lambda () (/ 7 8))) ((lambda () (define nine 9) (- nine 10))))
   
   '(apply f (1 2 3))
   
   '(let () (define x 5) (a (let ((a 1)) (set! a 2) (- 3 a))) (- b a) (c))
   
   '(let () (let () (let () (f ((lambda () (let () ((lambda x (f (lambda (g h . k) x)))) 1)))))))
   
   '((lambda (a) (lambda x (lambda (g . h) (let ((h 15)) h)))) a x g)
   
   '(begin (begin 5) (begin (display "abcde") (+ 6 7)) #t #f (display "Lorem Ipsum"))
   
   '(lambda (a b c d e f g h i j k lmnop q r stuv w . xyz) 
      xyz (lambda (k j) ((lambda () j) k)) (i h) g f free-var1 (e d c b ((((a))))))
      
   '(cond ((a 1) b)
	  ((= ((lambda a 2) (list 2)) 3) (caddr a))
	  (else (car x)))
	  
   '(lambda (a b c)
      (begin
      (begin
      (define foo (lambda (x) x ))
      (define goo (lambda (y) y )))
      (begin
      (define moo (lambda (x) x ))
      (define poo (lambda (y) y ))))
      (* (foo (poo (moo a)))
      (goo (* b c))))
	
  '(lambda (x) (set! x 3) (+ x 4))
  
  '(lambda (a b . x) 1)

))  

(define Comp161Ass3Tests
  (list
    '(lambda (a) (a 2))
    
    '(lambda (a b)
      (a (b 2)))
  
    '(lambda (a b)
      (a (a b)))
      
    '(lambda (a)
	(if (b? a)
	    (- a 1)
	    (b a)))
  
    '(lambda (a)
      (begin
	(define b (lambda (x)
		    (+ x a)))
	(define c (lambda (x)
		    (x a)))
	(c (b a))))
	
    '(x (lambda (y)
      (begin
	(define a (lambda (b)
		    (y b)))
	(define t (lambda(x)
		    (begin
		      (define h (lambda (j)
				  (x j y)))
		      h)))
	(t a))))
	
    '(lambda ( x y z zx xx)   
      (begin
      (define t1 
	(lambda (g t) 
		  (if (zero? y) zx (g t z))))
	(define y (lambda (y x)
		    (+ y x)))
	(define g (lambda (y f)
		    (y f))))
	(t1 (y z g) g))

    '(lambda (x)  
      (begin
	(define a
	  (lambda (x y z)
	    (begin 
	    (set! z z)
	    (set! z x))
	    (x y z)))
	(define b b)
	(define c (lambda (y)
		    (lambda(z)
		      (z x y))))
      (b y))
    (x b y a))
    
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  5)
	
	(* a (ff b) (b a))))
	
    '(lambda (a b c d e)
      (a (b c) (c b d) (a (b (c (d e))))))
      
    '(lambda (a) 
      (set! min4 (lambda (a) 
		    (set! min3 (lambda (a) 
				    (set! min2 (lambda (a) 
						  (- a 2))) 
				    (- (min2 a) 1))) 
		  (- (min3 a) 1))) 
      (+ (min4 a) a))
	
    '(lambda (c)
      (define f2 (lambda (x)
		      (+ x x (c x))))
      (if (not (eq? c 0))
	  (try that)        
	  (f2 (f2 c))))

    '(define fibonacci
      (memoize
	(lambda (n)
	  (if (< n 2)
	      1
	      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))
	      
    '((lambda (c)
      (+ c 5)) 7)
      
    '(let ([fact (lambda (n)
		  (if (= n 0)
		    1
		    (* n (fact (- n 1)))))])
      (fact 5))
      
    '(letrec 
      ((first 1) 
      (second 2) 
      (third 3) 
      (fourth (lambda (x) 
		(if (> x 0) 
		    (begin 
		      (+ 1 first) 
		      (+ 1 second)                    
		      (newline)
		      (fourth (- x 1)))
				      1))))
      (fourth (fourth 2)))
      
    '(define factorial 
      (lambda(n)
	(if (= n 0)
	  1
	  (* n (factorial (- n 1))))))
	  
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(define j 34)
	(begin 
	    (define moo (lambda (x)
			  (begin 
			    (define x 10)
			    (define y (lambda (x)(not x)))
			    (set! a (* a b c))))))
	(* (foo (goo (moo x)))
	(goo (* b c j)))))
    
    '(a b)
    
    '(lambda (x)(list (lambda (y) (set! x 1)) x))
    
    '(lambda (x y z)
      (lambda (y)
	((set! x 5)) (+ x y))
	(+ x y z))
	
    '(lambda (x y z)
      (lambda (y)
	((set! x 5) (set! y 6) (+ x y))
	(+ x y z)))
	
    '(x (lambda (y)
      (begin
	(set! a (lambda (b)
		    (y b)))
	(set! t (lambda(x)
		    (begin
		      (set! y (lambda (j)
				  (x j x)))
		      h)))
	(y a))))
	
    '(lambda (x)
      (x (lambda (y)
	    (x (lambda (y)
		(x (lambda (y) (set! x y))))))))
		
    '(lambda ( x y z zx xx)   
      (begin
      (define t1 
	(lambda (g t) 
		  (if (zero? y) zx (g t z))))
	(define y (lambda (y x)
		    (+ y x)))
	(define g (lambda (y f)
		    (y f))))
	(set! y x) (t1 (y z g) g))
	
    '(lambda (x)
      (set! x 6)
      (lambda (y) x))
      
    '(lambda (x)
      (set! x 6)
      (set! x 6)
      (set! x 6)
      (set! x 6)
      (lambda (y) x))
      
    '(lambda (x z)  
      (begin
	(define a
	  (lambda (g y)
	    (begin 
	    (set! z z)
	    (set! x z))
	    (x y z)))
	(define b b)
	(define c (lambda (f)
		    (lambda(g)
		      (z x y))))
      (b y))
    (x b y a))
    
    '(lambda (x)  
      (lambda (z)
	(set! x x)))
	
    '(lambda (x y)  
      (lambda (z)
	(set! x x)
	(set! y 2)))
	
    '(lambda (x y)
      (+ y 2)
      (set! y 2)
      (lambda (z)
	  (set! x x)))
	  
    '(define foo
      (lambda (x)    
	(begin
	  (define (poo x y) (goo (boo x y)))
	  (define (moo x) (* x x x))
	  (set! x (goo x))
	  (set! x (boo x))
	  (poo x (moo x)))))
	  
    '(lambda (x)
      (begin
	(define y 10)
	(set! x (* x x))    
	(if (even? x) y (- y x))))
	
    '(lambda (a) 
      (begin 
	(define a 4) 
	(define id1 
	  (lambda  (c b) a))) 
      (* a a))
      
    '(lambda (a b) 
      (begin 
	(define iszero (lambda (a b) 
			(or (zero? a) (zero? b))))
	(define min1 (lambda (a) (- a 1))) 
	(define min2 (lambda (b) (- (min1 b) 2)))) 
      (if (iszero a b) (min1 a) (min2 b)))
      
    '(lambda (t i)    
	(begin                            
	  (begin                           
	    (define l (lambda (x) (+ x x)))
	    (define o (lambda (y) (* i t))))
	  (begin                         
	    (define i (lambda (x) (+ x x)))
	    (define e (lambda (y) (- y)))))
	(* i t))
	
      '(lambda (a b c)
        (begin
  	(begin
  	  (begin 
  	    (define foo (lambda (x) (set! foo 4)(+ x x)))
  	    (define goo (lambda (y) 
  			  (define foo (* a b)) (* foo b)))))
  	(define t5 (lambda (x) (begin
  				(define x 6)
  							    (set! goo 5)
  							    (set! f 50)
  				(* a x))))
  	(begin 
  	  (define f (lambda (d)
  		      (* d d d )))
  	  (begin
  	    (define h (lambda (f g)
  			(if (or (even? g) (even? g) (even (+ f g))) 
  			    f g)))))
  	)    
  	(* (foo (goo (* a x)))
  	(f (* b c))))

      
    '(x (lambda (y)
	(begin
	  (define a (lambda (b)
		      (define a (lambda (b) b))
				      (set! a (lambda (b) b))
				      (set! t (lambda (b) b))
				      (set! h (lambda (b) b))
		      (y b)))
	  (define t (lambda(t)
		      (begin
			(define h (lambda (j)
				    (define y 7)
				    (t j y)))
			h)))
	  (t (a h)))))
	  
     '(lambda (a b c d e)
       (define f (lambda (x)
 		  (a (b c) (c b d) (a (b (c (d e)))))))
       (set! a 3)
       (set! b 5)
       (set! c a) #f)
		  
    '(define cse
      (lambda (exp)
	(let* ((res_aux (aux exp '()))
	      (res_let (make_let (car res_aux) (cadr res_aux) '() 1)))
	  (if (null? (cadr res_let))
	      exp
	      (let ((let_ribs (filter 
				(lambda (x) x)
				(map 
				  (lambda (rib)
				    (find (car rib) (cadr res_let)))
				  (cadr res_aux)))))
		(list 'let* let_ribs (car res_let)))))))
		
    '(set! are-parentheses-balanced-list?
      (lambda (are-parentheses-balanced-list?)
	      (lambda (exp num)
		    (set! are-parentheses-balanced-list? are-parentheses-balanced-list?)	  
		    (if (and (= num 0) (null? exp)) #t
			    (if (or (< num 0) (null? exp)) #f
				    (if (char=? (car exp) #\()
					    (are-parentheses-balanced-list? (cdr exp) (+ num 1))
					    (if (char=? (car exp) #\))
						    (are-parentheses-balanced-list? (cdr exp) (- num 1))
						    (are-parentheses-balanced-list? (cdr exp) num))))))))
						    
    '(define make-monitored (lambda(proc) 
                         (letrec
                               ((counter 0)
                                  (how-many-calls (lambda ( ) counter))
                                  (reset-count (lambda( ) (set! counter 0)))
                                  (compute (lambda (arg) 
                                             ( begin 
                                               (set! counter (+ counter 1)) (proc arg) )))
                                  (dispatch (lambda (op arg)
                                              (cond ((eq? op 'how-many-calls?) (how-many-calls))
                                                    ((eq? op 'reset-count) (reset-count))
                                                    ((eq? op 'compute) (compute arg))
                                                    (else 0)))))
                               dispatch)))
                               
    '(define make-sq-jump (lambda (lst)
                      (letrec ((main-func (lambda (make-sq-jump res-lst) 
											(lambda (a) 
											 (set! make-sq-jump make-sq-jump)
											 (set! res-lst res-lst)
											 (if (null? (cdr (memq a lst)))
                                             (list lst)											 
                                             (let ((res-lst (make-sq-jump (cdr (memq a lst)))))
                                               (append (list (append (list a )
                                                                       (filter (lambda (res-lst-element)
                                                                                 ( > (car res-lst-element) (* a a)))
                                                                               res-lst ))) 
                                                       res-lst )))))))
                             (main-func  (car lst)))))
                             
    '(define primitive-application? (lambda (primitive-application?)
								((lambda (app)
                                 (primitive-application? (car app))
								 (set! primitive-application? primitive-application?)))))
    
    '(define list-of-values
      (lambda (list-of-values)
	(lambda (exps env)
	  (if (no-operands? exps)
	      '()
	      (cons (env-eval (first-operand exps) env)
		    (list-of-values (rest-operands exps) env)))
	      (set! list-of-values (cdr list-of-values)))))
	      
    '(define (add-binding! binding)
      (add-binding-to-frame! binding (first-frame the-global-environment)))
      
    '(define x (lambda (x) (lambda () (set! x 5) x)))
    
    '(let ((counter 0)) 
      (set! inc-counter-by (lambda (n) (set! counter (+ counter n)))) 
      (set! inc-counter (lambda () (inc-counter-by 1))) 
      (set! counter-value (lambda () counter)))
      
    '(lambda (x) x)
    
    '(lambda (x) x x x)
    
    '(lambda (x y) (+ x y))
    
    '(lambda (y) (x y))
    
    '(lambda (x y z)
	(define x 5)
	(+ x y z))
	
    '(x (lambda (y)
	(begin
	  (define a (lambda (b)
		      (y b)))
	  (define t (lambda(x)
		      (begin
			(define h (lambda (j)
				    (x j y)))
			h)))
	  (t a))))
	  
    '((lambda (x)
	(x (lambda (y)
	      (x (lambda (y)
		  (x (lambda (y) y))))))))
		  
    '(lambda (x y)
      (x (y 6)))
      
    '(lambda (we)
      (if (are? all)
	  (free vars)))
	  
    '(lambda (x xx)
      (set! x xx)
      (- xxx x))
      
    '(lambda (x y z zx xx)
      (begin
	(define y (+ x 5))
	(set! y 8)
	xx)
      (if (zero? y) zx))
      
    '(lambda (x y z zx xx)   
      (begin
      (define t1 
	(lambda (g t) 
		  (if (zero? y) zx (g t z)))))
	t1)
	
    '(lambda ( x y z zx xx)   
      (begin
      (define t1 
	(lambda (g t) 
		  (if (zero? y) zx (g t z))))
	(define y (lambda (y x)
		    (+ y x)))
	(define g (lambda (y f)
		    (y f))))
	(t1 (y z g) g))
	
    '(lambda (x)  
	(begin
	  (define a
	    (lambda (x y z)
	      (begin 
	      (set! z z)
	      (set! z x))
	      (x y z)))
	  (define b b)
	  (define c (lambda (y)
		      (lambda(z)
			(z x y))))
	(b y))
      (x b y a))
      
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(define t5 (lambda (x) (begin
				(define x 6)
				(* a x))))
	(begin 
	  (define f (lambda (d)
		      (* d d (t5 d) )))
	  (begin
	    (define h (lambda (f g)
			(define or_p 
			  (lambda (x y) (or (even? x) (even? y) (even (+ x y)))))
			(define t6 (lambda (x) (begin
						(define a1 1)
						(define a2 2)
						(* 2 (+ a1 a2)))))
			(if (or_p f g) 
			    f (t6 g))))))
	)(+ (t5 6) (t6 7) (f 6)))
	
    '(lambda (x)
      (lambda (y)
	(lambda (x)
	  ((lambda (z)
	    (x y y)) x)
	  )))
	  
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  5)
	
	(* a (ff b) b a)))
	
    '(lambda (x y)
      (list (lambda () x)
	    (lambda (z) (set! y z))
	    (lambda (z) (set! x z))))
	    
    '(lambda (a) 
      (set! min4 (lambda (a) 
		    (set! min3 (lambda (a) 
				    (set! min2 (lambda (a) 
						  (- a 2))) 
				    (- (min2 a) 1))) 
		  (- (min3 a) 1))) 
      (+ (min4 a) a))
      
    '(test (lambda (x)
	(x (lambda (y x)
	      (x (lambda (y)
		  (x (lambda (y) y) test)))))) zero?)
		  
    '(lambda (a) (define a 5) a)
    
    '(lambda (a) (define add5 (lambda (b) (+ b 5))) (add5 a))
    
    '(lambda (a) (define id (lambda (b) b)) (define min1 (lambda (b) (- b 1))) (min1 (id a)))
    
    '(lambda (a b c) (define plus (lambda (a b) (+ a b))) (define minus (lambda (a b) (- a b))) (minus (plus a b) c))

    '(lambda (x)
      (begin
	(define y 10)
	(if (even? x) y (- y x))))
	
    '(lambda (x)
      (begin
	(define y 10)
	(set! x (* x x))    
	(if (even? x) y (- y x))))
	
    '(lambda (a) (begin (define a 4) (define id1 (lambda  (a b) a))) (* a a))
    
    '(lambda (a) (begin (define a 4) (define id1 (lambda  (a b) a))) (* (id1 a a) a))
    
    '(lambda (a b) (begin (define iszero (or (zero? a) (zero? b))) (define id1 (lambda (a b) a))) (if iszero 3 5))
    
    '(lambda (a b) (begin (define iszero (lambda (a b) (or (zero? a) (zero? b)))) (define min1 (lambda (a) (- a 1))) (define min2 (lambda (b) (- b 2)))) (if (iszero a b) (min1 a) (min2 b)))
    
    '(lambda (a) (define min4 (lambda (a) (define min3 (lambda (a) (define min2 (lambda (a) (- a 2))) (-(min2 a) 1))) (- (min3 a) 1))) (+ (min4 a) a))
    
    '(lambda (t i)    
      (begin                            
        (begin                           
          (define l (lambda (x) (+ x x)))
          (define o (lambda (y) (* y y))))
        (begin                         
          (define v (lambda (x) (+ x x)))
          (define e (lambda (y) (- y)))))
      (* i t))
      
    '(lambda (a)                                 
	  (begin                               
		(begin                            
		  (define force (lambda (x) (+ x x)))
		  (define the  (lambda (y) (* y y)))
		  (define with (lambda (x) (- x 4))))             
		(begin                             
		  (define may (lambda (x) (+ x x)))
		  (define be (lambda (y) (* 5 y))))
		(define you a))                         
	  (may (the (force (be (with you))))))
	  
    '(lambda (a b c)
	(begin
	  (begin
	    (begin 
	      (define foo (lambda (x) (+ x x)))
	      (define goo (lambda (y) 
			    (define foo (* a b)) (* foo b)))))
	  (begin 
	      (define moo (lambda (x)
			    (begin 
			      (define x 10)
			      (define y (lambda (x)(not x)))
			      (set! a (* a b c))))))
	  (* (foo (goo (moo x)))
	  (goo (* b c)))))
	  
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(define j 34)
	(begin 
	    (define moo (lambda (x)
			  (begin 
			    (define x 10)
			    (define y (lambda (x)(not x)))
			    (set! a (* a b c))))))
	(* (foo (goo (moo x)))
	(goo (* b c j)))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b))))))    
	(* (foo (goo (* a x)))
	(goo (* b c))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(define t5 (lambda (x) (begin
				(define x 6)
				(* a x))))
	(begin 
	  (define f (lambda (d)
		      (* d d d )))
	  (begin
	    (define h (lambda (f g)
			(if (or (even? g) (even? g) (even (+ f g))) 
			    f g)))))
	)    
	(* (foo (goo (* a x)))
	(f (* b c))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(define t5 (lambda (x) (begin
				(define x 6)
				(* a x))))
	(begin 
	  (define f (lambda (d)
		      (* d d (t5 d) )))
	  (begin
	    (define h (lambda (f g)
			(define or_p 
			  (lambda (x y) (or (even? x) (even? y) (even (+ x y)))))
			(define t6 (lambda (x) (begin
						(define a1 1)
						(define a2 2)
						(* 2 (+ a1 a2)))))
			(if (or_p f g) 
			    f (t6 g))))))
	)    
	(* (foo (goo (h a x)))
	(f (* b (t5 c)))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (define foo (lambda (x) (+ x x)))
	    (define goo (lambda (y) 
			  (define foo (* a b)) (* foo b)))))
	(begin 
	  (define f (lambda (d)
		      (* d d (foo d) )))
	  (begin
	    (define h (lambda (f g)
			(define or_p 
			  (lambda (x y) (or (even? x) (even? y) (even (+ x y)))))
			(define t6 (lambda (x) (begin
						(define a1 1)
						(define a2 2)
						(* 2 (+ a1 a2)))))
			(if (or_p f g) 
			    f (t6 g))))))
	)    
	(* (foo (goo (h a x)))
	(f (* b (f c)))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (begin
	      (begin
		(define x a))
	      (define xx b)
	      (define xxx (* b b))
	    )))
	(* x xx xxx b a)))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (begin
	      (begin
		(define x a))
	      (define xx b)
	      (define xxx (* b b))
	    )))
	(begin 
	  (define xxxx c)
	(* x xx xxx xxxx b a))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (begin 
	    (begin
	      (begin
		(define x a))
	      (define xx b)
	      (define xxx (* b b))
	    )))
	(begin 
	  (define xxxx c)
	  (define tt (lambda (d) (begin
				(define t 8) (* t 7))))                           
	(* x xx xxx (tt xxxx) b a))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  (begin 
	    (begin
	      (begin
		(define x a))
	      (define xx b)
	      (define xxx (* b b))
	    )))
	(begin 
	  (define xxxx c)
	  (define tt (lambda (d) (begin
				(define t 8) (* t 7))))
			      
	(* x xx (ff c) (tt xxxx) b a))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  (begin 
	    (begin
	      (begin
		(define x a))
	      (define xx b)
	      (define xxx (* b b))
	      (define t1 (lambda (x)
			  (define t2 (lambda(x)
					(define t3 (lambda (x)
						    (define t4 (lambda (x)x))
						    (- 5 (t4 x))))
					(+ x (t3 7))))
			  (- (t2 a) a)))
	      
	    )))
	(begin 
	  (define xxxx c)
	  (define tt (lambda (d) (begin
				(define t 8) (* t 7))))
			      
	(* x (t1 xx) (ff c) (tt xxxx) b a))))
	
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  5)
	
	(* a (ff b) b a)))
	
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      (- t g)))
	  (ff 5)))
	(ff b))
	
    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c (t c)))))
		      4))
	  (ff 5)))
	(ff b))

    '(lambda (a b c)
      (begin
	(begin
	  (define ff (lambda (g)
		      (begin
			(define t (+ 6 g))
			(define u (lambda (c) (* c t ))))
		      (u t)))
	  (ff 5))
	(ff b)))
	
    '(lambda (a) (begin (define b (+ 87 a))) b)
    
    '(lambda (a)(define b 6) b)
    
    '(lambda (a) (set b 2) 5)
    
    '(lambda (a) (begin 5 (set b 2) #t))
    
    '(lambda (a) (box-set b 2) 5)
    '(lambda (a) (box-get b 2) 5)
    
    '(lambda (a) (begin 5 (box-set b 2) (box-get a) #t))    
	
))

(define GiladWinterfeldTests
  (list
	;;test1
        '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))

        ;;test2
        '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

        ;;test3
        '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))

        ;;test4
        '(lambda (x) (x x))

        ;;test5
        '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))

        ;;test6
        '(lambda (a  b) (lambda (c) (+ a b c)))

        ;;test7
        '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

        ;;test8
        '(let ((a 0)) (list (lambda () a) (lambda () (set! a (+ a 1))) (lambda (b) (set! a b))))

        ;;test9
        '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) (define odd? (lambda (n) (and (positive? n) (even? (- n 1))))) (even? e)))

        ;;test10
        '(+ 1 2)
        
        ;;test11
        '(lambda (x . y) (lambda (x) y x (set! x 1 )))
        
        ;;test12
        '(lambda (x  y) (lambda () y x (set! x 1 )))
        
        ;;test13
        '(lambda (x ) (lambda (x) y  (set! x 1 )))
        
        ;;test14
        '(lambda (x  ) (lambda (x) y x (set! z 1 )))
        
        ;;test15
        '(lambda (x ) (lambda x  x (set! x 1 )))
        
        ;;test16
        '(lambda (x ) x (lambda (a b) (set! x 1 )))
        
        ;;test17
        '(lambda x x (lambda (a b) (set! x 1 )))
        
        '(lambda x a (lambda (a b) (set! x 1 )))
        
        ;;test18
        '(lambda (a) (+ x a (lambda b (+ x a b (lambda (c . d) (+ x a b c d (lambda (e f g) (+ x a b c d e f g))))))))
        
        ;;test19
        '(+ 1 2 (lambda () (if (+ (- 1)) (if ( + (set! a (+ 1))) (or (+ 1) (+ 2) ) (begin (+ 1) (lambda () (+ 2)) (+ 3))) (lambda a (+ a)))))
        
        ;;test20
        '((lambda () (+ ((lambda () a)) ((lambda () b)) ((lambda () ((lambda () c)))))))
        
        ;;test21
        '(+ a b c (lambda (a b c) (+ a b c (lambda (a) (+ a b c (lambda(b) (+ a b c (lambda(c) (+ a b c (lambda (x) a b c) )))))))))
        
        ;;test22
        '(+ a b c(lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c (lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c)))))))))))))
        
        ;;test23
        '(lambda(x)  (set! x 1) (lambda() (set! t (+ x 1) )))
        
        ;;test24
        '(lambda () (define a 1) (lambda() (define b 2) 'body0) )
        
        ;;test25
        '(lambda (a . b) (define x 1) 1)
        
        ;;test26
        '(lambda a (define x 1) x)
        
        ;;test27
        '(lambda (a  b ) (define x 1) (lambda (a) (define a 4) (lambda a (define a 3) 1)))

        ;;test28
        '(lambda (a) (define p (lambda (ab) (define s 1) a)) b)
        
        ;;test29
        '(if (lambda (a . rr) (define a (lambda z (define z 1) 2)) 3) (+ (begin (+ 4) (lambda (c t) (define r 1) (define g 2) ((lambda() 'hello))))) (lambda () (or (+ 5) (+ (- 6)) (if (+ 6) (set! a (+ 4)) (or ( + 7) (+ ( - 8)))))))

        ;;test30
        '(lambda () (define (a x) x) b)
        
        ;;test31
        '(lambda () (define x 1) (define (y) 2) 'body)

        ;;test32
        '((lambda a a))
        
        ;;test33
        '((lambda () (define x 1) x))
        
        ;;test34
        '(lambda () (define (a . b) a) 1)
        
        ;;test35
        '(let* ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
        
        ;;test36
        '(letrec ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))

        ;;test37
        '(let ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
        
        ;;test38
        '(let ([a (let ([b 1]) (define b 2) b)]) (define a 1) a)

        ;;test39
        '(lambda () (or (+ 1) (or ( + 2) (+ 3) ) (+ 4)))
        
        ;;test40
        '(or (+ 1) (or ( + 2) (+ 3) ) (+ 4))
        
        ;;test41  (negative test)
        '((lambda () 1) 2 3 4)
        
        ;;test42
        '(lambda a (lambda b (define x 2) 2) #f)
        
        ;;test43
        '(lambda ()
	  (lambda ()
	    (lambda (x)
	      (list (lambda () (lambda () x)) (lambda (x) (set! x 1))))))

	;;test44
	'(lambda () (define (a b . c) 3) x)
	
	;;test45
	'(lambda (x)
	x
	(set! x 1)
	(lambda (x) (lambda () x (set! x 1))))

	;;test46
	'(lambda (x)
	  (lambda ()
	    (set! x (lambda (z) (lambda () z (set! z 1))))
	    x))
	    
	;;test47
	'(lambda (x)
	  (lambda ()
	    y
	    (set! x (+ 1 x (lambda (z) (lambda () z (set! z 1)))))))
	    
	;;test48
	'(lambda() (if (lambda a (define x (lambda () x)) 8 ) (+ (- 9)) (lambda(x) (lambda () 
	  (set! x (+ 1 x (lambda (x) (lambda () x (set! x 1)))))))))
    
))    


(define EladZoharTests
  (list
'(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))
	   
'(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))	   

'(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

'(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

'(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

'(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

'(define list-head
  (lambda (s n)
    (cond ((null? s) '())
	  ((zero? n) '(#\space #\e #\t #\c))
	  (else (cons (car s)
		  (list-head (cdr s) (- n 1)))))))

'(define <end-of-input>
  (lambda (s ret-match ret-none)
    (if (null? s)
	(ret-match #t '())
	(ret-none '()))))

'(define const
  (lambda (pred?)
    (lambda (s ret-match ret-none)
      (cond ((null? s) (ret-none '()))
	    ((pred? (car s)) (ret-match (car s) (cdr s)))
	    (else (ret-none '()))))))

'(define <epsilon>
  (lambda (s ret-match ret-none)
    (ret-match '() s)))

'(define caten
  (letrec ((binary-caten
	    (lambda (p1 p2)
	      (lambda (s ret-match ret-none)
		(p1 s
		    (lambda (e1 s)
		      (p2 s
			  (lambda (e2 s)
			    (ret-match (cons e1 e2) s))
			  ret-none))
		    ret-none))))
	   (loop
	    (lambda (ps)
	      (if (null? ps)
		  <epsilon>
		  (binary-caten (car ps)
				(loop (cdr ps)))))))
    (lambda ps
      (loop ps))))

'(define <fail>
  (lambda (s ret-match ret-none)
    (ret-none '())))

'(define disj
  (letrec ((binary-disj
	    (lambda (p1 p2)
	      (lambda (s ret-match ret-none)
		(p1 s ret-match
		    (lambda (w1)
		      (p2 s ret-match
			  (lambda (w2)
			    (ret-none `(,@w1 ,@w2)))))))))
	   (loop
	    (lambda (ps)
	      (if (null? ps)
		  <fail>
		  (binary-disj (car ps)
			       (loop (cdr ps)))))))
    (lambda ps
      (loop ps))))

'(define delay
  (lambda (thunk)
    (lambda (s ret-match ret-none)
      ((thunk) s ret-match ret-none))))

'(define star
  (lambda (p)
    (disj (pack-with (caten p (delay (lambda () (star p))))
		     cons)
	  <epsilon>)))

'(define plus
  (lambda (p)
    (pack-with (caten p (star p))
	       cons)))

'(define times
  (lambda (<p> n)
    (if (zero? n)
	<epsilon>
	(pack-with
	 (caten <p> (times <p> (- n 1)))
	 cons))))

'(define pack
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (f e) s)) ret-none))))

'(define pack-with
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (apply f e) s)) ret-none))))

'(define diff
  (lambda (p1 p2)
    (lambda (s ret-match ret-none)
      (p1 s
	  (lambda (e w)
	    (p2 s (lambda _ (ret-none '()))
		(lambda (w1) (ret-match e w))))
	  ret-none))))

'(define maybe
  (lambda (p)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s) (ret-match `(#t ,e) s))
	 (lambda (w) (ret-match `(#f #f) s))))))

'(define maybe?
  (lambda (?result)
    (car ?result)))

'(define maybe->value
  (lambda (?result)
    (cadr ?result)))

'(define fence
  (lambda (p pred?)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s)
	   (if (pred? e)
	       (ret-match e s)
	       (ret-none '())))
	 ret-none))))

'(define otherwise
  (lambda (p message)
    (lambda (s ret-match ret-none)
      (p s
	 ret-match
	 (let ((marker
		(format "-->[~a]"
		  (list->string
		   (list-head s *marker-length*)))))
	   (lambda (w) (ret-none `(,@w ,message ,marker))))))))

'(define ^char
  (lambda (char=?)
    (lambda (character)
      (const
       (lambda (ch)
	 (char=? ch character))))))

'(define char (^char char=?))

'(define char-ci (^char char-ci=?))

'(define ^word
  (lambda (char)
    (lambda (word)
      (apply caten (map char (string->list word))))))

'(define word (^word char))

'(define word-ci (^word char-ci))

'(define ^word-suffixes
  (lambda (char)
    (letrec ((loop
	      (lambda (s)
		(if (null? s)
		    <epsilon>
		    (maybe
		     (caten (char (car s))
			    (loop (cdr s))))))))
      (lambda (suffix)
	(loop (string->list suffix))))))

'(define word-suffixes (^word-suffixes char))

'(define word-suffixes-ci (^word-suffixes char-ci))

'(define ^word+suffixes
  (lambda (word-suffixes)
    (lambda (prefix suffix)
      (caten (word prefix)
	     (word-suffixes suffix)))))

'(define word+suffixes (^word+suffixes word-suffixes))

'(define word+suffixes-ci (^word+suffixes word-suffixes-ci))

'(define ^one-of
  (lambda (char)
    (lambda (word)
      (apply disj (map char (string->list word))))))

'(define one-of (^one-of char))

'(define one-of-ci (^one-of char-ci))

'(define ^range
  (lambda (char<=?)
    (lambda (char-from char-to)
      (const
       (lambda (ch)
	 (and (char<=? char-from ch)
	      (char<=? ch char-to)))))))

'(define range (^range char<=?))

'(define range-ci (^range char-ci<=?))

'(define <any-char> (const (lambda (ch) #t)))

'(define <any> <any-char>)

'(define ^<separated-exprs>
  (lambda (<expr> <sep>)
    (new (*parser <expr>)
	 
	 (*parser <sep>)
	 (*parser <expr>)
	 (*caten 2)
	 (*pack-with (lambda (_sep expr) expr))
	 *star
	 
	 (*caten 2)
	 (*pack-with cons)
	 done)))

'(define continue
  (lambda (ds cs)
    (with cs
      (lambda (c . cs)
	(c ds cs)))))

'(define new
  (lambda cs
    (continue '() cs)))

'(define done
  (lambda (ds cs)
    (with ds
      (lambda (parser . ds)
	(if (null? ds)
	    parser
	    (error 'done
		   (format "The parser stack still contains ~a parsers!"
		     (length ds))))))))

'(define *parser
  (lambda (p)
    (lambda (ds cs)
      (continue `(,p . ,ds) cs))))

'(define unary
  (lambda (f-unary)
    (lambda (ds cs)
      (with ds
	(lambda (d . ds)
	  (continue `(,(f-unary d) . ,ds) cs))))))

'(define *delayed
  (lambda (thunk)
    (lambda (ds cs)
      (continue `(,(delay thunk) . ,ds) cs))))

'(define binary
  (lambda (f-binary)
    (lambda (ds cs)
      (with ds
	(lambda (d2 d1 . ds)
	  (continue `(,(f-binary d1 d2) . ,ds) cs))))))

'(define *dup
  (lambda (ds cs)
    (with ds
      (lambda (d1 . ds)
	(continue `(,d1 ,d1 . ,ds) cs)))))

'(define *swap
  (lambda (ds cs)
    (with ds
      (lambda (d1 d2 . ds)
	(continue `(,d2 ,d1 . ,ds) cs)))))

'(define *star (unary star))

'(define *plus (unary plus))

'(define *diff (binary diff))

'(define *pack (lambda (f) (unary (lambda (p) (pack p f)))))

'(define *pack-with (lambda (f) (unary (lambda (p) (pack-with p f)))))

'(define *fence (lambda (pred?) (unary (lambda (p) (fence p pred?)))))

'(define *guard (lambda (pred?) (unary (lambda (p) (fence p pred?)))))

'(define split-list
  (lambda (s n ret-s1+s2)
    (if (zero? n)
	(ret-s1+s2 '() s)
	(split-list (cdr s) (- n 1)
		    (lambda (s1 s2)
		      (ret-s1+s2 (cons (car s) s1) s2))))))

'(define nary
  (lambda (f-n-ary n)
    (lambda (ds cs)
      (split-list ds n
       (lambda (sgra ds)
	 (continue
	  `(,(apply f-n-ary (reverse sgra)) . ,ds) cs))))))

'(define *caten (lambda (n) (nary caten n)))

'(define *disj (lambda (n) (nary disj n)))

'(define *maybe (unary maybe))

'(define *otherwise
  (lambda (string)
    (unary
     (lambda (p)
       (otherwise p string)))))

'(define *times
  (lambda (n)
    (unary
     (lambda (<p>)
       (times <p> n)))))

'(define not-followed-by
  (lambda (<p1> <p2>)
    (new (*parser <p1>)
	 (*parser <p2>) *maybe
	 (*caten 2)
	 (*pack-with
	  (lambda (e1 ?e2)
	    (with ?e2
	      (lambda (found-e2? _)
		`(,e1 ,found-e2?)))))
	 (*guard
	  (lambda (e1+found-e2?)
	    (with e1+found-e2?
	      (lambda (_ found-e2?)
		(not found-e2?)))))
	 (*pack-with
	  (lambda (e1 _) e1))
	 done)))

'(define *transformer
  (lambda (^<p>)
    (unary (lambda (<p>) (^<p> <p>)))))

'(define test-string
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s)
	      `((match ,e)
		(remaining ,(list->string s))))
	    (lambda (w) `(failed with report: ,@w)))))

'(define test
  (lambda (parser s)
    (parser s
	    (lambda (e s)
	      `((match ,e)
		(remaining ,s)))
	    (lambda (w) `(failed with report: ,@w)))))

'(define file->string
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
		(lambda ()
		  (let ((e (read-char input)))
		    (if (eof-object? e)
			(begin
			  (close-input-port input)
			  '())
			(cons e (run)))))))
	(list->string (run))))))

'(define read-stdin-to
  (lambda (end-of-input)
    (let ((end-of-input-list (string->list end-of-input)))
      (letrec ((state-init
		(lambda (seen)
		  (let ((ch (read-char)))
		    (cond ((eof-object? ch)
			   (error 'read-stdin-to
			     (format "Marker ~a not reached"
			       end-of-input)))
			  ((char=? ch (car end-of-input-list))
			   (state-seen seen `(,ch) (cdr end-of-input-list)))
			  (else (state-init `(,ch ,@seen)))))))
	       (state-seen
		(lambda (seen-before seen-now end-of-input-list-rest)
		  (if (null? end-of-input-list-rest)
		      (list->string
		       (reverse seen-before))
		      (let ((ch (read-char)))
			(cond ((eof-object? ch)
			       (format "Marker ~a not reached"
				 end-of-input))
			      ((char=? ch (car end-of-input-list-rest))
			       (state-seen seen-before
					   `(,ch ,@seen-now)
					   (cdr end-of-input-list-rest)))
			      (else (state-init
				     `(,ch ,@seen-now ,@seen-before)))))))))
	(state-init '())))))

'(define ?
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

'(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

'(define compose-patterns
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

'(define (test-error loc fmt . args)
  (let ([s (tests-state)]
        [msg (parameterize ([current-inspector (test-inspector)]
                            [print-struct #t])
               (apply format fmt args))]
        [top? (car loc)]
        [loc (apply make-srcloc (cdr loc))])
    (if (and (not top?) (mpair? s))
      (begin (set-mcdr! s (cons (cons msg loc) (mcdr s)))
             (set-mcar! s (add1 (mcar s))))
      (raise (make-exn:test (string-append "Test failure: " msg)
                            (current-continuation-marks)
                            (list loc))))))
'(define (test-ok loc expr)
  (let ([s (tests-state)])
    (if (and (not (car loc)) (mpair? s))
      (set-mcar! s (add1 (mcar s)))
      (printf "Test passed\n"))))

'(define (in-test-context! loc)
  (unless (or (car loc) (tests-state))
    (error 'test "invalid use (not in `run-tests' or in the REPL)")))

'(define (test-1 val expr loc)
  (in-test-context! loc)
  (if ((test-postprocess) val)
    (test-ok loc expr)
    (test-error loc "~.s is false" expr)))
'(define (test-2 val1 expr1 val2 expr2 loc)
  (in-test-context! loc)
  (parameterize ([current-inspector (test-inspector)])
    (let ([val1 ((test-postprocess) val1)]
          [val2 ((test-postprocess) val2)])
      (if (equal? val1 val2)
        (test-ok loc expr1)
        (test-error loc "~.s: expected ~e, got ~e" expr1 val2 val1)))))

))

(display (format "\033[1mComp171 - Ass3 Tests\033[0m\n================================\n"))

(runAllTests
  (list      
      (cons "Comp161 Ass3 Tests" Comp161Ass3Tests)
      (cons "Complex Tests" Tests)  
      (cons "Gilad Winterfeld Tests" GiladWinterfeldTests) 
      (cons "Elad Zohar Tests" EladZoharTests) 
))