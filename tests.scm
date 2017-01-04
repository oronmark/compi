(load "compiler.scm")

(define display-green-with-number
    (lambda (counter)
        (display (format 
            (string-append
                "\033[1;32m"
                "===================== All ("
                (number->string counter)
                (if (= counter 1)
                    ") TEST"
                    ") TESTS" )
                    " SUCCEEDED =====================\033[0m \n")))))
                    
(define display-wrong-test
    (lambda(x)
        (display (format "\033[1;31m==================== WRONG TEST ===================="))(display (format "\033[1;39m"))(newline)(newline)
        (display (format "\033[0;31mTest:"))(display (format "\033[1;39m"))(newline)(display (car x))(newline)(newline)
        (display (format "\033[0;31mExpected:"))(display (format "\033[1;39m"))(newline)(display (car (cdr x)))(newline)(newline)
        (display (format "\033[0;31mActual:"))(display (format "\033[1;39m"))(newline)(display (car (cdr (cdr x))))(newline)(newline)))

(define bad-summary
    (lambda()
        (let*
            ((wrong-count
                (length (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results))))
             (succsess-count
                (- (length tests) wrong-count)))
            (display (format 
                (string-append
                    "\033[1;32m"
                    "==================== "
                    (number->string succsess-count)
                    " / "
                    (number->string (+ wrong-count succsess-count))
                    (if (= succsess-count 1)
                        " TEST"
                        " TESTS" )
                    " SUCCEEDED ====================\033[0m \n")))
            (newline)
            (display (format 
                (string-append
                    "\033[1;31m"
                    "====================== "
                    (number->string wrong-count)
                                        " / "
                    (number->string (+ wrong-count succsess-count))
                    (if (= wrong-count 1)
                        " TEST"
                        " TESTS" )
                    " FAILED ======================\033[0m \n"))))))

(define test-all
    (lambda ()
        (if (null? (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results)))
            ((lambda ()
                (display-green-with-number (length tests))))
            (begin
                (map
                    display-wrong-test
                    (filter (lambda(x) (not (equal? #t x))) (test-func tests mayer-results)))
                (bad-summary)
                (void)))))

(define test-func
    (lambda (lst r)
        (if (null? lst)
            '()
            (cons (if (equal? (annotate-tc
                        (pe->lex-pe
                            (box-set
                                (remove-applic-lambda-nil
                                    (eliminate-nested-defines
                                        (parse (car lst)))))))
                                        (car r))
                        #t
                        (list   (car lst)
                                (car r)
                                (annotate-tc
                                    (pe->lex-pe
                                        (box-set
                                            (remove-applic-lambda-nil
                                                (eliminate-nested-defines
                                                    (parse (car lst)))))))))
                    (test-func (cdr lst) (cdr r))))))
                    
(define tests
    (list
        ;; mayer's
        
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
        
        ;'(lambda x a (lambda (a b) (set! x 1 )))
        
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
        '(lambda () (define x 1) (define (x) 2) 'body)

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

        
))

(define mayer-results
    (list
            ;;test1
            '(lambda-simple
                (f)
                (tc-applic
                    (lambda-simple
                    (x)
                    (tc-applic
                        (bvar f 0 0)
                        ((lambda-var
                        s
                        (tc-applic
                            (fvar apply)
                            ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0)))))))
                    ((lambda-simple
                    (x)
                    (tc-applic
                        (bvar f 0 0)
                        ((lambda-var
                            s
                            (tc-applic
                            (fvar apply)
                            ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0))))))))))

            ;;test2
            '(applic
                (fvar x)
                ((lambda-simple
                    (x)
                    (tc-applic
                    (pvar x 0)
                    ((lambda-simple
                        ()
                        (tc-applic
                            (bvar x 0 0)
                            ((lambda-simple
                            ()
                            (tc-applic (bvar x 1 0) ((bvar x 1 0))))))))))))

            ;;test3
            '(def (fvar fact)
                (lambda-simple
                (n)
                (if3 (applic (fvar zero?) ((pvar n 0)))
                        (const 1)
                        (tc-applic
                        (fvar *)
                        ((pvar n 0)
                            (applic
                            (fvar fact)
                            ((applic (fvar -) ((pvar n 0) (const 1))))))))))

            ;;test4
            '(lambda-simple (x) (tc-applic (pvar x 0) ((pvar x 0))))

            ;;test5
            '(def (fvar fact)
                (lambda-simple
                (n)
                (if3 (applic (fvar zero?) ((pvar n 0)))
                        (const 1)
                        (tc-applic
                        (fvar *)
                        ((pvar n 0)
                            (applic
                            (fvar fact)
                            ((applic (fvar -) ((pvar n 0) (const 1))))))))))

            ;;test6
            '(lambda-simple
                (a b)
                (lambda-simple
                    (c)
                    (tc-applic
                    (fvar +)
                    ((bvar a 0 0) (bvar b 0 1) (pvar c 0)))))

            ;;test7
            '(applic
                (fvar x)
                ((lambda-simple
                    (x)
                    (tc-applic
                    (pvar x 0)
                    ((lambda-simple
                        ()
                        (tc-applic
                            (bvar x 0 0)
                            ((lambda-simple
                            ()
                            (tc-applic (bvar x 1 0) ((bvar x 1 0))))))))))))

            ;;test8
            '(applic
                (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (box (pvar a 0)))
                        (tc-applic
                            (fvar list)
                            ((lambda-simple () (box-get (bvar a 0 0)))
                            (lambda-simple
                                ()
                                (box-set
                                (bvar a 0 0)
                                (applic (fvar +) ((box-get (bvar a 0 0)) (const 1)))))
                            (lambda-simple (b) (box-set (bvar a 0 0) (pvar b 0))))))))
                ((const 0)))

            ;;test9
            '(def (fvar my-even?)
                (lambda-simple
                (e)
                (tc-applic
                    (lambda-simple
                    (even? odd?)
                    (seq ((set (pvar even? 0) (box (pvar even? 0)))
                            (set (pvar odd? 1) (box (pvar odd? 1)))
                            (box-set
                                (pvar even? 0)
                                (lambda-simple
                                (n)
                                (or ((applic (fvar zero?) ((pvar n 0)))
                                        (tc-applic
                                        (box-get (bvar odd? 0 1))
                                        ((applic
                                            (fvar -)
                                            ((pvar n 0) (const 1)))))))))
                            (box-set
                                (pvar odd? 1)
                                (lambda-simple
                                (n)
                                (if3 (applic (fvar positive?) ((pvar n 0)))
                                    (tc-applic
                                        (box-get (bvar even? 0 0))
                                        ((applic (fvar -) ((pvar n 0) (const 1)))))
                                    (const #f))))
                            (tc-applic (box-get (pvar even? 0)) ((bvar e 0 0))))))
                    ((const #f) (const #f)))))

                
                
            ;;test10
            '(applic (fvar +) ((const 1) (const 2)))

            ;;test11
            '(lambda-opt  (x)  y  (lambda-simple    (x)    (seq ((bvar y 0 1) (pvar x 0) (set (pvar x 0) (const 1))))))

            ;;test12
            '(lambda-simple (x y) (seq ((set (pvar x 0) (box (pvar x 0))) (lambda-simple () (seq ((bvar y 0 1) (box-get (bvar x 0 0)) (box-set (bvar x 0 0) (const 1))))))))

            ;;test13
            '(lambda-simple
                (x)
                (lambda-simple
                    (x)
                    (seq ((fvar y) (set (pvar x 0) (const 1))))))

            
            ;;test14
            '(lambda-simple
                (x)
                (lambda-simple
                    (x)
                    (seq ((fvar y) (pvar x 0) (set (fvar z) (const 1))))))

            ;;test15
            '(lambda-simple
                (x)
                (lambda-var
                    x
                    (seq ((pvar x 0) (set (pvar x 0) (const 1))))))

            
            ;;test16
            '(lambda-simple
                (x)
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-get (pvar x 0))
                        (lambda-simple (a b) (box-set (bvar x 0 0) (const 1))))))
            ;;test17
            '(lambda-var
                x
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-get (pvar x 0))
                        (lambda-simple (a b) (box-set (bvar x 0 0) (const 1))))))
                
                
            ;;test18
            '(lambda-simple
                (a)
                (tc-applic
                    (fvar +)
                    ((fvar x)
                    (pvar a 0)
                    (lambda-var
                        b
                        (tc-applic
                        (fvar +)
                        ((fvar x)
                            (bvar a 0 0)
                            (pvar b 0)
                            (lambda-opt
                            (c)
                            d
                            (tc-applic
                                (fvar +)
                                ((fvar x) (bvar a 1 0) (bvar b 0 0) (pvar c 0) (pvar d 1)
                                (lambda-simple
                                    (e f g)
                                    (tc-applic
                                    (fvar +)
                                    ((fvar x) (bvar a 2 0) (bvar b 1 0) (bvar c 0 0)
                                        (bvar d 0 1) (pvar e 0) (pvar f 1)
                                        (pvar g 2)))))))))))))

            
            ;;test19
            '(applic
                (fvar +)
                ((const 1)
                    (const 2)
                    (lambda-simple
                    ()
                    (if3 (applic (fvar +) ((applic (fvar -) ((const 1)))))
                        (if3 (applic
                                (fvar +)
                                ((set (fvar a) (applic (fvar +) ((const 1))))))
                                (or ((applic (fvar +) ((const 1)))
                                    (tc-applic (fvar +) ((const 2)))))
                                (seq ((applic (fvar +) ((const 1)))
                                    (lambda-simple () (tc-applic (fvar +) ((const 2))))
                                    (tc-applic (fvar +) ((const 3))))))
                        (lambda-var a (tc-applic (fvar +) ((pvar a 0))))))))

                
            ;;test20
            '(applic (fvar +) ((fvar a) (fvar b) (fvar c)))

            
            ;;test21
            '(applic
                (fvar +)
                ((fvar a)
                    (fvar b)
                    (fvar c)
                    (lambda-simple
                    (a b c)
                    (tc-applic
                        (fvar +)
                        ((pvar a 0)
                        (pvar b 1)
                        (pvar c 2)
                        (lambda-simple
                            (a)
                            (tc-applic
                            (fvar +)
                            ((pvar a 0)
                                (bvar b 0 1)
                                (bvar c 0 2)
                                (lambda-simple
                                (b)
                                (tc-applic
                                    (fvar +)
                                    ((bvar a 0 0)
                                    (pvar b 0)
                                    (bvar c 1 2)
                                    (lambda-simple
                                        (c)
                                        (tc-applic
                                        (fvar +)
                                        ((bvar a 1 0)
                                            (bvar b 0 0)
                                            (pvar c 0)
                                            (lambda-simple
                                            (x)
                                            (seq ((bvar a 2 0)
                                                    (bvar b 1 0)
                                                    (bvar c 0 0))))))))))))))))))

            
            ;;test22
            '(applic
            (fvar +)
            ((fvar a)
                (fvar b)
                (fvar c)
                (lambda-var
                a
                (tc-applic
                    (fvar +)
                    ((pvar a 0)
                    (fvar b)
                    (fvar c)
                    (lambda-opt
                        (a)
                        b
                        (tc-applic
                        (fvar +)
                        ((pvar a 0)
                            (pvar b 1)
                            (fvar c)
                            (lambda-simple
                            (a)
                            (tc-applic
                                (fvar +)
                                ((pvar a 0)
                                (bvar b 0 1)
                                (fvar c)
                                (lambda-var
                                    a
                                    (tc-applic
                                    (fvar +)
                                    ((pvar a 0)
                                        (bvar b 1 1)
                                        (fvar c)
                                        (lambda-opt
                                        (a)
                                        b
                                        (tc-applic
                                            (fvar +)
                                            ((pvar a 0)
                                            (pvar b 1)
                                            (fvar c)
                                            (lambda-simple
                                                (a)
                                                (tc-applic
                                                (fvar +)
                                                ((pvar a 0)
                                                    (bvar b 0 1)
                                                    (fvar c)))))))))))))))))))))
            ;;test23                                  
            '(lambda-simple
                (x)
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-set (pvar x 0) (const 1))
                        (lambda-simple
                        ()
                        (set (fvar t)
                                (applic (fvar +) ((box-get (bvar x 0 0)) (const 1))))))))
                        
            ;;test24
            '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (const 1))
                            (lambda-simple
                            ()
                            (tc-applic
                                (lambda-simple
                                (b)
                                (seq ((set (pvar b 0) (const 2)) (const body0))))
                                ((const #f)))))))
                    ((const #f))))
            
            ;;test25
            '(lambda-opt
                (a)
                b
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (const 1))))
                    ((const #f))))

            ;;test26
            '(lambda-var
                a
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (pvar x 0))))
                    ((const #f))))

            ;;test27
            '(lambda-simple
                (a b)
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1))
                            (lambda-simple
                            (a)
                            (tc-applic
                                (lambda-simple
                                (a)
                                (seq ((set (pvar a 0) (const 4))
                                        (lambda-var
                                            a
                                            (tc-applic
                                            (lambda-simple
                                                (a)
                                                (seq ((set (pvar a 0) (const 3))
                                                    (const 1))))
                                            ((const #f)))))))
                                ((const #f)))))))
                    ((const #f))))

            ;;test28
            '(lambda-simple
                (a)
                (tc-applic
                    (lambda-simple
                    (p)
                    (seq ((set (pvar p 0)
                                (lambda-simple
                                (ab)
                                (tc-applic
                                    (lambda-simple
                                    (s)
                                    (seq ((set (pvar s 0) (const 1)) (bvar a 2 0))))
                                    ((const #f)))))
                            (fvar b))))
                    ((const #f))))


            ;;test29
            '(if3 (lambda-opt
                (a)
                rr
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0)
                                (lambda-var
                                    z
                                    (tc-applic
                                    (lambda-simple
                                        (z)
                                        (seq ((set (pvar z 0) (const 1)) (const 2))))
                                    ((const #f)))))
                            (const 3))))
                    ((const #f))))
                (applic
                (fvar +)
                ((seq ((applic (fvar +) ((const 4)))
                        (lambda-simple
                            (c t)
                            (tc-applic
                            (lambda-simple
                                (r g)
                                (seq ((set (pvar r 0) (const 1))
                                        (set (pvar g 1) (const 2))
                                        (const hello))))
                            ((const #f) (const #f))))))))
                (lambda-simple
                ()
                (or ((applic (fvar +) ((const 5)))
                        (applic (fvar +) ((applic (fvar -) ((const 6)))))
                        (if3 (applic (fvar +) ((const 6)))
                            (set (fvar a) (applic (fvar +) ((const 4))))
                            (or ((applic (fvar +) ((const 7)))
                                    (tc-applic
                                    (fvar +)
                                    ((applic (fvar -) ((const 8))))))))))))
                                    
                                    
            ;;test30
            '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (lambda-simple (x) (pvar x 0)))
                            (fvar b))))
                    ((const #f))))
                    
            ;;test31
            '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (x x)
                    (seq ((set (pvar x 0) (const 1))
                            (set (pvar x 0) (lambda-simple () (const 2)))
                            (const body))))
                    ((const #f) (const #f))))

            
            ;;test32
            '(applic (lambda-var a (pvar a 0)) ())

            ;;test33
            '(applic
                (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (pvar x 0))))
                ((const #f)))
                
            ;;test34
            '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (box (pvar a 0)))
                            (box-set (pvar a 0) (lambda-var b (box-get (bvar a 0 0))))
                            (const 1))))
                    ((const #f))))
                    
            ;;test35
            '(applic
                (lambda-simple
                    (a)
                    (tc-applic
                    (lambda-simple
                        (b)
                        (tc-applic
                        (lambda-simple
                            (c)
                            (tc-applic
                            (lambda-simple
                                (d)
                                (tc-applic
                                (fvar +)
                                ((bvar a 2 0) (bvar b 1 0) (bvar c 0 0) (pvar d 0))))
                            ((const 4))))
                        ((const 3))))
                    ((const 2))))
                ((const 1)))
                
            ;;test36
            '(applic
                (lambda-simple
                    (a b c d)
                    (seq ((set (pvar a 0) (const 1))
                        (set (pvar b 1) (const 2))
                        (set (pvar c 2) (const 3))
                        (set (pvar d 3) (const 4))
                        (tc-applic
                            (fvar +)
                            ((pvar a 0) (pvar b 1) (pvar c 2) (pvar d 3))))))
                ((const #f) (const #f) (const #f) (const #f)))
            
            ;;test37
            '(applic
                (lambda-simple
                    (a b c d)
                    (tc-applic
                    (fvar +)
                    ((pvar a 0) (pvar b 1) (pvar c 2) (pvar d 3))))
                ((const 1) (const 2) (const 3) (const 4)))
                
            ;;test38
            '(applic
                (lambda-simple
                    (a)
                    (tc-applic
                    (lambda-simple
                        (a)
                        (seq ((set (pvar a 0) (const 1)) (pvar a 0))))
                    ((const #f))))
                ((applic
                    (lambda-simple
                    (b)
                    (tc-applic
                        (lambda-simple
                        (b)
                        (seq ((set (pvar b 0) (const 2)) (pvar b 0))))
                        ((const #f))))
                    ((const 1)))))
                    
            ;;test39
            '(lambda-simple
                ()
                (or ((applic (fvar +) ((const 1)))
                        (or ((applic (fvar +) ((const 2)))
                            (applic (fvar +) ((const 3)))))
                        (tc-applic (fvar +) ((const 4))))))
            
            ;;test40
            '(or ((applic (fvar +) ((const 1)))
                (or ((applic (fvar +) ((const 2)))
                        (applic (fvar +) ((const 3)))))
                (applic (fvar +) ((const 4)))))

            ;;test41
            '(applic
                (lambda-simple () (const 1))
                ((const 2) (const 3) (const 4)))

            ;;test42
            '(lambda-var
                a
                (seq ((lambda-var
                        b
                        (tc-applic
                            (lambda-simple
                            (x)
                            (seq ((set (pvar x 0) (const 2)) (const 2))))
                            ((const #f))))
                        (const #f))))



))


(define test-all2
    (test-all)

    )



      
  








