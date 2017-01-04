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

(define tests-counter 1)

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
		(display (format "~s) ~s\n" tests-counter input))
		(set! tests-counter (+ 1 tests-counter))
		(let ((actual-output (test-func input))
		      (expected-output (full-cycle input)))
			(cond ((equal? actual-output expected-output)
				(display (format "\033[1;32m Success! ☺ \033[0m \n\n")) #t)
				(else 
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
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

;; passed
;- failed
; not checked

(define Tests
  (list
  ;; '(lambda (x) x)
    
  ;;  '(lambda (x y) x y (lambda (a b c) a b c))
    
  ;;  '(lambda (x y) (lambda (a b c) a b c) x y)
				    
  ;;  '(lambda (x y) ((lambda (a b c) a b c) 5 6 7) x y)
				   
  ;;  '(lambda (a b c d e) ((lambda (a b c) (lambda (z e1) (e1 5))) 5 6 7))
							  
  ;;  '(lambda (a b c d e) ((lambda (a b c) (lambda () 5))))

  ;; '(lambda (x) (define a 5) b)   
	    
  ;;  '(lambda (x) (lambda (y) (define x 10) (a 5)))  

  ;; '(lambda (x) (define y (lambda () (define a 5) 4)) 1)    
     
  ;;  '(lambda (x) (define a 5) (lambda (y) (define x 10) (a 5)))    
    ;;;;;don't remove comment;;;;;;'(lambda (x) (define a 5) (define a 123) (lambda (y) (define x 10) (a 5)) 2)
     
  ;- '(lambda (z) (define a 5) (define b 123) (lambda (y) (define x 10)
	;-(define x1 (lambda (abc) (define a 56) (define x1 10) (+ 1 2))) (f 32 45 'a)) (a 5))

     
  ;;  '(define x (lambda (x) x))

        
  ;;   '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) #t))


   ;;   '(define odd? (lambda (n) (and (positive? n) (even? (- n 1)))))


   ;;   '(even? e)
      
   ;-   '(lambda x (lambda (a . b) a (lambda (b) b c (f (lambda (y) (define a 5) a))))) ;;error with part 7



;;  '(lambda (x) ((f (lambda (z) z)) (lambda (y) y)))


   ;;   '(lambda (x) (lambda (y) (lambda (z) z)))


   ;  '(lambda (x) (lambda (y) (lambda (z) (define x 1) (define y 2) (define z 3) y)))

       
     ;; '(lambda (x . y) 1)

	 
   ;;  '(lambda (x . y) (lambda (x) y x (set! x 1) (* x 1)))


   ;-   '(lambda (x) x (f (lambda (y) (define a 5) y))) ;;error with part 3

			  
  ;;  '(let ((a 0))
	;;  (list
	;;  (lambda () a)
	;;  (lambda () (set! a (+ a 1)))
	;;  (lambda (b) (set! a b))))	

	  
  ;;  '(let* ((c 0)
	;;  (a (box c)))
	;;  (list
	;;  (lambda () a)
	;;  (lambda () (set! a (+ a 1)))
	;;  (lambda (b) (set! a b))))


 ;;   '(lambda (a . b) a)

       
;;    '(lambda (a . b) (lambda () a))

       
 ;;   '(lambda (a . b) (begin (lambda () a) (set! a 5)))

		
 ;-   '(let ((a 0) (c 1))
;-	  (list
;-	  (lambda () a)
;-	  (lambda () (set! a (+ a 1)))
;-	  (lambda (b) (set! a b))
;-	  (lambda () c)
;-	  (lambda () (set! c (+ a 1)))
;-	  (lambda (b) (set! c b))    
;-	))

	  
 ;;   '(lambda (a b) (begin a (set! a b) (lambda () a) (lambda (a b) (+ a b))))

		  
 ;;   '(lambda (a) a)

       
;;    '(lambda (a b) a c (lambda (a . b) b))


;    '(lambda (a b) a (lambda (b . c) b))  ;;;error part7


 ;;   '(lambda () (+ 1 2))


 ;;  '(lambda (+) (+ 1 2))


  ;; '(lambda a a)

       
 ;;  '(a (lambda a a) (lambda (a) a) a (lambda (a) (lambda () a) a))


 ;;   '((lambda (a) a) (lambda (b) b d) (lambda (c) c))


;;   '((lambda (a) a) (lambda (b) b) (lambda (c) c) d)

	    
 ;;   '(lambda (a b) (lambda (c) a))

       
 ;;   '(lambda (a b) (lambda (c) a (lambda () b)))


  ;;  '(lambda () a (lambda () (lambda () b)))

    
;;    '(f (g (h (a (b (abc (lambda () a (lambda () (lambda () b)))))))))


 ;;   '(lambda (a b) (lambda (c) a (lambda () b (lambda (d) c))))


;;   '(f (lambda (a b) (lambda (c) a (lambda () (lambda (d) (lambda x a))))))


;;    '((lambda (a) (begin a (lambda () a) (lambda (a) a))))


 ;;   '(lambda (a b) c d (lambda () c d))


 ;;   '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

;;   '(lambda (a b) (lambda (c) (+ a b c)))


 ;;   '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) 


 ;;  '(set! x (f 1))

      
;;    '(or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

		  
;;    '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))		    

			
;;   '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))) (a (b 1)))


 ;;   '(and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

	
 ;;   '(let ((a 1)) (and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))	

	  
 ;;   '(begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))

               
 ;   '(((lambda (a b c d) (begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) ((lambda () ((lambda () d))))))))))) ;;error


 ;  '((lambda () 5)) ;;error with rdundent applic

      
 ;  '(lambda (x) (x x) (display "asaf"))


;  '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))

		    
;    '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))		    

	
 ;   '(lambda (x) (x x))

	
;  '((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))

      
 ;  '(((lambda () f)) ((lambda () g)) ((lambda () h)) (z (m c (d ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))))
   
;   '(lambda(x) (lambda (y) (set! x 1) (lambda () x)))
   
;   '(or 3 4 (lambda (x) (define x 3) 5))
   
;   '(lambda (a) a (lambda (b c) (set! a 4) (+ a b) (lambda () (set! b 8))))
   
 ;  '(lambda (x) (lambda () (set! x 1)))
   
;   '(lambda x (lambda () (set! x 1)))
   
;   '(lambda (x . y) (lambda () (set! x 1)))
   
;   '(or 1 (lambda (x) (x x)) (lambda (y) (y y)))
   
;   '(lambda (a) (lambda () (set! a 3) b)) 
   
;   '(lambda (a) (set! a 3) a)   
   
 ;  '(a (lambda (a) (a (lambda (b) (a b (lambda (a) (a b (lambda (c) (a b c)))))))))
   
;   '(define x (+ 1 (* 3 4) (- 5 6)))
   
 ;  '(* (* 1 2) 3 (+ 4 5) ((lambda () (/ 7 8))) ((lambda () (define nine 9) (- nine 10))))

))  

(define GiladWinterfeldTests
  (list
	;;test1
;        '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))

        ;;test2
   ;     '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

        ;;test3
 ;       '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))

        ;;test4
  ;      '(lambda (x) (x x))
;
        ;;test5
  ;      '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))

        ;;test6
 ;       '(lambda (a  b) (lambda (c) (+ a b c)))

        ;;test7
  ;      '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))

        ;;test8
 ;       '(let ((a 0)) (list (lambda () a) (lambda () (set! a (+ a 1))) (lambda (b) (set! a b))))

        ;;test9
  ;      '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) (define odd? (lambda (n) (and (positive? n) (even? (- n 1))))) (even? e)))

        ;;test10
 ;       '(+ 1 2)
        
        ;;test11
  ;      '(lambda (x . y) (lambda (x) y x (set! x 1 )))
  ;;      
        ;;test12
   ;     '(lambda (x  y) (lambda () y x (set! x 1 )))
        
        ;;test13
 ;       '(lambda (x ) (lambda (x) y  (set! x 1 )))
        
        ;;test14
  ;      '(lambda (x  ) (lambda (x) y x (set! z 1 )))
        
        ;;test15
  ;      '(lambda (x ) (lambda x  x (set! x 1 )))
        
        ;;test16
 ;       '(lambda (x ) x (lambda (a b) (set! x 1 )))
        
        ;;test17
  ;      '(lambda x x (lambda (a b) (set! x 1 )))
        
   ;     '(lambda x a (lambda (a b) (set! x 1 )))
        
        ;;test18
   ;     '(lambda (a) (+ x a (lambda b (+ x a b (lambda (c . d) (+ x a b c d (lambda (e f g) (+ x a b c d e f g))))))))
  ;      
        ;;test19
  ;      '(+ 1 2 (lambda () (if (+ (- 1)) (if ( + (set! a (+ 1))) (or (+ 1) (+ 2) ) (begin (+ 1) (lambda () (+ 2)) (+ 3))) (lambda a (+ a)))))
        
        ;;test20
  ;      '((lambda () (+ ((lambda () a)) ((lambda () b)) ((lambda () ((lambda () c)))))))
        
        ;;test21
  ;      '(+ a b c (lambda (a b c) (+ a b c (lambda (a) (+ a b c (lambda(b) (+ a b c (lambda(c) (+ a b c (lambda (x) a b c) )))))))))
        
        ;;test22
  ;      '(+ a b c(lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c (lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c)))))))))))))
        
        ;;test23
   ;     '(lambda(x)  (set! x 1) (lambda() (set! t (+ x 1) )))
        
        ;;test24
  ;      '(lambda () (define a 1) (lambda() (define b 2) 'body0) )
        
        ;;test25
   ;     '(lambda (a . b) (define x 1) 1)
        
        ;;test26
   ;     '(lambda a (define x 1) x)
        
        ;;test27
   ;     '(lambda (a  b ) (define x 1) (lambda (a) (define a 4) (lambda a (define a 3) 1)))
;
        ;;test28
   ;     '(lambda (a) (define p (lambda (ab) (define s 1) a)) b)
        
        ;;test29
    ;    '(if (lambda (a . rr) (define a (lambda z (define z 1) 2)) 3) (+ (begin (+ 4) (lambda (c t) (define r 1) (define g 2) ((lambda() 'hello))))) (lambda () (or (+ 5) (+ (- 6)) (if (+ 6) (set! a (+ 4)) (or ( + 7) (+ ( - 8)))))))

        ;;test30
    ;    '(lambda () (define (a x) x) b)
        
        ;;test31
     ;   '(lambda () (define x 1) (define (x) 2) 'body)

        ;;test32
    ;    '((lambda a a))
        
        ;;test33
     ;   '((lambda () (define x 1) x))
        
        ;;test34
      ;  '(lambda () (define (a . b) a) 1)
        
        ;;test35
    ;    '(let* ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
        
        ;;test36
     ;   '(letrec ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))

        ;;test37
      ;  '(let ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
        
        ;;test38
       ; '(let ([a (let ([b 1]) (define b 2) b)]) (define a 1) a)

        ;;test39
    ;    '(lambda () (or (+ 1) (or ( + 2) (+ 3) ) (+ 4)))
        
        ;;test40
     ;   '(or (+ 1) (or ( + 2) (+ 3) ) (+ 4))
        
        ;;test41  (negative test)
      ;  '((lambda () 1) 2 3 4)
        
        ;;test42
    ;    '(lambda a (lambda b (define x 2) 2) #f)
        
        ;;test43
     ;   '(lambda ()
	;  (lambda ()
	 ;   (lambda (x)
	  ;    (list (lambda () (lambda () x)) (lambda (x) (set! x 1))))))

	;;test44
;	'(lambda () (define (a b . c) 3) x)
	
	;;test45
;	'(lambda (x)
;	x
;	(set! x 1)
;	(lambda (x) (lambda () x (set! x 1))))

	;;test46
;	'(lambda (x)
;	  (lambda ()
;	    (set! x (lambda (z) (lambda () z (set! z 1))))
;	    x))
	    
	;;test47
;	'(lambda (x)
;	  (lambda ()
;	    y
;	    (set! x (+ 1 x (lambda (z) (lambda () z (set! z 1)))))))
	    
	;;test48
;	'(lambda() (if (lambda a (define x (lambda () x)) 8 ) (+ (- 9)) (lambda(x) (lambda () 
;	  (set! x (+ 1 x (lambda (x) (lambda () x (set! x 1)))))))))
    
))        

(display (format "\033[1mComp171 - Ass3 Tests\033[0m\n================================\n"))

(runAllTests
  (list
      (cons "Gilad Winterfeld Tests" GiladWinterfeldTests) 
      (cons "Complex Tests" Tests)           
))
