(include "pc.scm")
(include "pattern-matcher.scm")
(include "qq.scm")

(define *reserved-words*
'(and begin cond define do else if lambda
  let let* letrec or quasiquote unquote
  unquote-splicing quote set!))

;; returns true if lst is a proper list, false otherwise
(define guard-proper-list
	(lambda (lst)
		(cond
			((list? lst) #t))))

;; returns true if var is not a reserved word and a symbol, false otherwise
(define guard-reserved-words
	(lambda (var)
		(if (not (symbol? var))
			#f
			(andmap (lambda (x)
					(not (symbol=? var x))) *reserved-words*)))
			)
;; return true if var is (not a reserved word and a symbol) or (list)
(define guard-reserved-words-list
	(lambda (var)
		(if (not (symbol? var))
			#t
			(andmap (lambda (x)
					(not (symbol=? var x))) *reserved-words*)))
			)

;;returns true is expr is not a list, false otherwise
(define guard-not-list
	(lambda (expr)
		(not (list? expr))))

;;If list's tails is nill it is a proper list!
(define guard-improper-list
  (lambda (lst)
    (if (equal? (list-tail lst 1) '())
      #f
      (and (andmap (lambda (elem) (and (symbol? elem) (guard-reserved-words elem))) (get-all-but-last lst '() ))
           (find-dups (get-all-but-last lst '()))
           (and (symbol? (cdr (last-pair lst))) (guard-reserved-words (cdr (last-pair lst))))
           ))))

(define simple-const?
	(lambda (c)
		(or (null? c) (vector? c) (boolean? c) (char? c) (number? c) (string? c))))

(define proper-list?
  (lambda (lst)
    (if (null? lst)
      #t
      (and (pair? lst)
           (proper-list? (cdr lst))))
    ))

(define *void-object*
	`(const ,(void)))

(define get-all-but-last
     (lambda (improper  all-but-last)
        (if (pair? (cdr improper))
            (get-all-but-last (cdr improper) (cons (car improper) all-but-last))
             (cons (car improper) all-but-last)
             )))

(define guard-vars-list
  (lambda (vars)
    (andmap (lambda (var) (and (symbol? var) (guard-reserved-words var))) vars)
    ))

(define find-dups-helper
  (lambda (lst elem help)
    (if (null? lst)
    help
    (if (eq? (car lst) elem)
      (find-dups-helper (cdr lst) elem (append help '(elem)))
      (find-dups-helper (cdr lst) elem help)
    ))))

; returns true if there are no dups and false if there are dups
(define find-dups
  (lambda (lst)
    (andmap (lambda (elem)
              (eq? (length (find-dups-helper lst elem '())) 1)) lst)
    ))

(define guard-mit-def
  (lambda (lst)
    (guard-reserved-words (car lst))
    ))

(define flatten-begin
  (lambda (expr)
    (cond ((null? expr) '())
          ((not (pair? (car expr))) (cons (car expr) (flatten-begin (cdr expr))))
          ((eq? (caar expr) 'begin) (append (flatten-begin (cdar expr)) (flatten-begin (cdr expr))))
          (else  (cons (car expr) (flatten-begin (cdr expr)))))
    ))

(define and->if
  (lambda (exprs)
        (if (null? (cddr exprs))
          `(if ,(car exprs) ,(cadr exprs) #f) ;you have reached the last of the list
          `(if ,(car exprs)
             ,(and->if (cdr exprs))
             #f))
    ))


(define cond->if-one-expr
  (lambda (listOfPairs)
    `(if ,(caar listOfPairs) (begin ,@(cdar listOfPairs)))
    ))


(define cond->if
  (lambda (listOfPairs)
    (if (null? (cdr listOfPairs))
        (cond->if-one-expr listOfPairs)
        (if (null? (cddr listOfPairs))
          (if (eq? (caadr listOfPairs) 'else)
            `(if ,(caar listOfPairs) (begin ,@(cdar listOfPairs)) (begin ,@(cdadr listOfPairs))) ;;with else
            `(if ,(caar listOfPairs) (begin ,@(cdar listOfPairs)) (if ,(caadr listOfPairs)       ;;no else
                                                               (begin ,@(cdadr listOfPairs) ))))
          `(if ,(caar listOfPairs) (begin ,@(cdar listOfPairs)) ,(cond->if (cdr listOfPairs))))   ;;more then 2 pairs
    )))

(define let->applic
  (lambda (listOfPairs body bodies)
    (let ((vars (fold-right (lambda (pair acc) (cons (car pair) acc)) '() listOfPairs))
          (vals (fold-right (lambda (pair acc) (cons (cadr pair) acc)) '() listOfPairs)))
      (if (find-dups vars)
       `((lambda ,vars ,body ,@bodies) ,@vals)
        (error #f "vars have duplicates in them")
      ))
    ))

(define let*->applic
  (lambda (listOfPairs body bodies)
    (let ((vars (fold-right (lambda (pair acc) (cons (car pair) acc)) '() listOfPairs))
          (vals (fold-right (lambda (pair acc) (cons (cadr pair) acc)) '() listOfPairs)))
        (let*-helper vars vals body bodies)
      )
    ))

(define let*-helper
  (lambda (vars vals body bodies)
      (if (null? (cdr vars))
        `((lambda ,vars ,body ,@bodies) ,@vals)
        `((lambda (,(car vars)) ,(let*-helper (cdr vars) (cdr vals) body bodies)) ,(car vals))
        )
    ))

(define letrec->applic
  (lambda (listOfPairs body bodies)
    (let ((vars (fold-right (lambda (pair acc) (cons (car pair) acc)) '() listOfPairs))
          (vals (fold-right (lambda (pair acc) (cons (cadr pair) acc)) '() listOfPairs)))
         `((lambda ,vars ,@(letrec-seq-helper listOfPairs) (,(letrec-body-helper body bodies))) ,@(map (lambda(pair) #f) listOfPairs))
         )))

(define letrec-seq-helper
  (lambda (listOfPairs)
        (map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) listOfPairs)
    ))

(define letrec-body-helper
 (lambda (body bodies)
   `(lambda () ,body ,@bodies)
   ))

(define guard-cond-rib
  (lambda (rib)
    ; (display "\n")
    ; (display rib)
    ; (display "\n")
    (and (pair? rib) (not (null? (cdr rib))))
    ))

(define guard-cond-rest-ribs
  (lambda (ribs)
    (andmap guard-cond-rib ribs)
    ))

(define parse
   (let ((run
      (compose-patterns
      	;;;;;;;;;;;;const;;;;;;;;;;
	    (pattern-rule
	      (? 'c simple-const?)
	      (lambda (c) `(const ,c)))
	    (pattern-rule
	      `(quote ,(? 'c))
	      (lambda (c) `(const ,c)))
	    ;;;;;;;;;;;;variable;;;;;;;;;;
	    (pattern-rule                     ;;check ascci output intead of regular output
	      (? 'var guard-reserved-words)
	      (lambda (var) `(var ,var)))
	    ;;;;;;;;;;;;if;;;;;;;;;;
	    (pattern-rule
	    	`(if ,(? 'test) ,(? 'then) ,(? 'else))
	    	(lambda (test then else)
	    		`(if3 ,(parse test) ,(parse then) ,(parse else))))
	    (pattern-rule
	    	`(if ,(? 'test) ,(? 'then))
	    	(lambda (test then)
	    		`(if3 ,(parse test) ,(parse then) ,*void-object*)))
	    ;;;;;;;;;;;;application;;;;;;;;;;
	    (pattern-rule
	    	`(,(? 'application guard-reserved-words-list) . ,(? 'argList))
	    	(lambda (application argList)
	    		`(applic ,(parse application) ,(map parse argList))))
            ;;;;;;;;;;;;or;;;;;;;;;;
	    (pattern-rule
	    	`(or . ,(? 'exprList))
	    	(lambda (exprList)
              (cond ((null? exprList) (parse #f))
                    ((null? (cdr exprList)) (parse (car exprList)))
                    (else `(or ,(map parse exprList)))
              )
          ))
	    ;;;;;;;;;;;;begin;;;;;;;;;;
	    (pattern-rule
	    	 `(begin . ,(? 'expers))
	    	(lambda (expers)
          (if (null? expers)
              *void-object*
              (if (null? (cdr expers))
                  (parse (car expers))
                  `(seq ,(map parse (flatten-begin expers))))
          )
          ))
            ;;;;;;;;;;;lambda-simple;;;;;;;;;;
	    (pattern-rule
	    	 `(lambda ,(? 'vars proper-list? guard-vars-list find-dups) ,(? 'body ) . ,(? 'bodies ))
	    	(lambda (vars body bodies)
                        (if (null? bodies)
                        `(lambda-simple ,vars ,(parse body))
                        `(lambda-simple ,vars ,(parse `(begin ,body ,@bodies))))
                  ))
            ;;;;;;;;;;;;lambda-opt;;;;;;;;;;
            (pattern-rule
	    	 `(lambda  ,(? 'vars pair? guard-improper-list)  ,(? 'body) . ,(? 'bodies))
	    	(lambda (vars body bodies)
                  (if (null? bodies)
                    `(lambda-opt ,(reverse (get-all-but-last vars '())) ,(cdr (last-pair vars)) ,(parse body))
                    `(lambda-opt ,(reverse (get-all-but-last vars '())) ,(cdr (last-pair vars)) ,(parse `(begin ,body ,@bodies))))
                    ))
            ;;;;;;;;;;;;lambda-variadic;;;;;;
	    (pattern-rule
	    	 `(lambda ,(? 'vars symbol? guard-reserved-words ) ,(? 'body) . ,(? 'bodies) )
                (lambda (vars body bodies)
                 (if (null? bodies)
                        `(lambda-var ,vars ,(parse body))
                        `(lambda-var ,vars ,(parse `(begin ,body ,@bodies))))
                 ))
            ;;;;;;;;;;;;define;;;;;;;;;;;;
            (pattern-rule
                `(define ,(? 'var symbol? guard-reserved-words),(? 'val))
                (lambda (var val)
                  `(def ,(parse var) ,(parse val))
                  ))
            ;;;;;;;;;;;;define-mit;;;;;;;;;;;;
            (pattern-rule
                `(define ,(? 'var guard-mit-def) . ,(? 'exprs))
                (lambda (var exprs)
                  `(def ,(parse (car var)) ,(parse `(lambda ,(cdr  var) ,@exprs)))
                  ))
            ;;;;;;;;;;;;set!;;;;;;;;;;;;
            (pattern-rule
              `(set! ,(? 'var symbol? guard-reserved-words) ,(? 'expr))
               (lambda (var expr)
                 `(set ,(parse var) ,(parse expr))
                ))
            ;;;;;;;;;;;;and-macro-expansion;;;;;;;;;;;;
            (pattern-rule
              `(and . ,(? 'exprs))
               (lambda (exprs)
                      (cond ((null? exprs) (parse #t))
                      ((null? (cdr exprs)) (parse (car exprs)))
                      (else (parse (and->if (cons (car exprs) (cdr exprs)))))
              )
                ))
            ;;;;;;;;;;;;cond-macro-expansion;;;;;;;;;;;;
            (pattern-rule
              `(cond ,(? 'expr guard-cond-rib) . ,(? 'exprs guard-cond-rest-ribs))
               (lambda (expr exprs)
                (parse (cond->if (cons expr exprs)))
                ))
            ;;;;;;;;;;;;let-macro-expansion;;;;;;;;;;;;
            (pattern-rule
              `(let ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
              (lambda (listOfPairs body bodies)
                (parse (let->applic listOfPairs body bodies))
                ))
            ;;;;;;;;;;;;let*-macro-expansion;;;;;;;;;;;;
            (pattern-rule
              `(let* ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
              (lambda (listOfPairs body bodies)
                (if (null? listOfPairs)
                  (parse `((lambda () ,body ,@bodies)))
                  (parse (let*->applic listOfPairs body bodies))
                )))
            ;;;;;;;;;;;;letrec-macro-expansion;;;;;;;;;;;;
            (pattern-rule
              `(letrec ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
              (lambda (listOfPairs body bodies)
                (if (null? listOfPairs)
                  (parse `((lambda () ((lambda () ,body ,@bodies)))))
                  (parse (letrec->applic listOfPairs body bodies))
                )))
            ;;;;;;;;;;;;quasiqoute;;;;;;;;;;;;
            (pattern-rule
             `(quasiquote ,(? 'e) . ,(? 'expers))
               (lambda (e expers)
                 (parse (expand-qq e))
                 ))
            )))
        (lambda (sexpr)
          (run sexpr (lambda() "ERROR")))
   ))



(define <digit-0-9>
  (range #\0 #\9)
  )

(define <digit-1-9>
  (range #\1 #\9)
  )

(define <char-a-f>
  (range #\a #\f)
  )

(define <Natural>
  (new (*parser <digit-0-9>) *plus
       (*pack (lambda (x)
		(string->number (list->string x))))
       done))

(define <Boolean>
  (new (*parser (word "#f"))
       (*pack (lambda (_) #f))
       (*parser (word "#t"))
       (*pack (lambda(_)  #t))
       (*disj 2)
       done))

(define <VisibleSimpleChar>
  (new (*parser (range #\! #\~))

       done))

(define ^<named-char>
  (lambda (str ch)
    (new (*parser (word str))
   (*pack (lambda (_) ch))
  done)))

(define <NamedChar>
  (new
       (*parser (^<named-char> "lambda" (integer->char 955)))
       (*parser (^<named-char> "newline" #\newline))
       (*parser (^<named-char> "nul" #\nul))
       (*parser (^<named-char> "page" #\page))
       (*parser (^<named-char> "return" #\return))
       (*parser (^<named-char> "space" #\space))
       (*parser (^<named-char> "tab" #\tab))

       (*disj 7)
  done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <Natural>)
       (*disj 3)
       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
	   		(lambda (num div den)
	  			(/ num den)))
       done))

(define <Number>
	(new
		(*parser <Fraction>)
		(*parser <Integer>)
		(*disj 2)
		done))

(define <HexChar>
	(new
		(*parser <digit-0-9>)
		(*parser <char-a-f>)

		(*disj 2)
	done))

(define <StringHexChar>
	(new
		(*parser (char #\\))
		(*parser (char #\x))
		(*parser <HexChar>) *star
		(*parser (char #\;))
		(*caten 4)
				(*pack-with (lambda (bs x h np)
					(integer->char (string->number (list->string `(,@h))16))))
	done))

(define <HexUnicodeChar>
  (new (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*caten 2)
			 (*pack-with (lambda (x h)
          (integer->char (string->number (list->string `(,@h))16)))
        )
  done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	done)))

(define <StringMetaChar>
	(new
    (*parser (^<meta-char> "\\\"" #\"))
    (*parser (^<meta-char> "\\" #\\))
		(*parser (^<meta-char> "\t" #\tab))
		(*parser (^<meta-char> "\f" #\page))
		(*parser (^<meta-char> "\n" #\newline))
		(*parser (^<meta-char> "\r" #\return))

		(*disj 6)
		done))

(define <StringLiteralChar>
  (new
      (*parser <any-char>)
      (*parser (char #\\))
      *diff
    done)
  )

  (define <PreChar>
    (new
        (*parser <VisibleSimpleChar>)
        (*parser <HexUnicodeChar>)
        (*parser <NamedChar>)
        (*disj 3)
    done))

(define <CharPrefix>
  (new
      (*parser (char #\#))
      (*parser (char #\\))
      (*caten 2)

       done))

(define <Char>
  (new
      (*parser <CharPrefix>)
      (*parser <VisibleSimpleChar>)
      (*parser <HexUnicodeChar>)
      (*parser <NamedChar>)
      (*disj 3)

      (*caten 2)

      (*pack-with (lambda (pre ch)
          ch))
    done))

(define <StringChar>
  (new
      (*parser <StringMetaChar>)
      (*parser <StringLiteralChar>)
      (*parser <StringHexChar>)

      (*disj 3)
    done))

(define <String>
  (new
      (*parser (char #\"))

      (*parser <StringChar>)
      (*parser (char #\"))
      *diff
      *star

      (*parser (char #\"))
      (*caten 3)

      (*pack-with
         (lambda (open-delim chars close-delim)
              (list->string chars)))
       done))

(define <SymbolChar>
  (new
      (*parser (range #\0 #\9))
      (*parser(range #\a #\z))
      (*parser(range #\A #\Z))
      (*parser (char #\!))
      (*parser (char #\$))
      (*parser (char #\^))
      (*parser (char #\*))
      (*parser (char #\-))
      (*parser (char #\_))
      (*parser (char #\=))
      (*parser (char #\+))
      (*parser (char #\<))
      (*parser (char #\>))
      (*parser (char #\?))
      (*parser (char #\/))
      (*disj 15)
    done))

(define <Symbol>
  (new
      (*parser <SymbolChar>) *plus
      (*pack (lambda (str)
            (string->symbol (list->string str))))
     done))

(define <ProperList>
  (new
      (*parser (char #\())
      (*delayed (lambda () <Sexpr>) ) *star
      (*parser (char #\)))
      (*caten 3)
    done))

(define <ImproperList>
  (new
      (*parser (word "("))
      (*delayed (lambda ()  <Sexpr>)) *plus
      (*parser (word "."))
      (*delayed (lambda ()  <Sexpr>))
      (*parser (word ")"))
      (*caten 5)

      (*pack-with
         (lambda (open-delim chars-plus dot chars  close-delim)
              `(,@chars-plus . ,chars)))
    done))

(define <Quoted>
  (new
    (*parser (char #\'))
    (*delayed (lambda () <Sexpr>) )
    (*caten 2)
  done))

(define <QuasiQuoted>
  (new
    (*parser (char #\`))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
  done))

(define <Unquoted>
  (new
    (*parser (char #\,))
    (*delayed (lambda () <Sexpr>) )
    (*caten 2)

  done))

(define <UnquotedAndSpliced>
  (new
    (*parser (char #\,))
    (*parser (char #\@))
    (*delayed (lambda () <Sexpr>) )
    (*caten 3)

    done))

(define <Vector>
  (new
  (*parser (char #\#))
  (*parser (char #\())
  (*delayed (lambda () <Sexpr>) ) *star
  (*parser (char #\)))
  (*caten 4)
  (*pack-with
        (lambda (asterics open-delim chars-star  close-delim)
         (vector (car chars-star))))
  done))

  (define <InfixPrefixExtenstionPrefix>
    (new
      (*parser (word "##"))
      (*parser (word "#%"))
      (*disj 2)
      done))

  (define <InfixSymbol>
    (new
      (*parser <Symbol>)
      (*parser (char #\+))
      *diff
      (*parser (char #\-))
      *diff
      (*parser (char #\*))
      *diff
      (*parser (char #\^))
      *diff
      (*parser (char #\/))
      *diff
      (*parser (word "**"))
      *diff
    done))

  (define <PowOp>
    (new
      (*parser (char #\^))
      (*parser (word "**"))
      (*disj 2)
      done))

  (define <MulDivOp>
    (new
      (*parser (char #\*))
      (*parser (char #\/))
      (*disj 2)
      done))

  (define <AddSubOp>
    (new
      (*parser (char #\+))
      (*parser (char #\-))
      (*disj 2)
      done))

  (define <InfixAddSub>
    (new
      (*delayed (lambda () <InfixMulDiv>))

      (*parser <AddSubOp>)
      (*delayed (lambda () <InfixMulDiv>))
      (*caten 2)
     (*pack-with
       (lambda (sign rest)
          (lambda (first)
            `(,(string->symbol (string sign)) ,first ,rest)
          )))

      *star
      (*caten 2)
     (*pack-with (lambda (first lambda_rest)
            (fold-left (lambda (op elem)
                            (elem op)) first lambda_rest)
          ))
      done))

  (define <InfixMulDiv>
    (new
      (*delayed (lambda () <InfixPow>))
      (*parser <MulDivOp>)
      (*delayed (lambda () <InfixPow>))
      (*caten 2)

     (*pack-with
       (lambda (sign rest)
          (lambda (first)
            `(,(string->symbol (string sign)) ,first ,rest)
          )))

      *star
      (*caten 2)
      (*pack-with (lambda (first lambda_rest)
            (fold-left (lambda (op elem)
                            (elem op)) first lambda_rest)
          ))
      done))

  (define <InfixPow>
    (new
      (*delayed (lambda () <End>))

      (*parser <PowOp>)
      (*delayed (lambda () <End>))
      (*caten 2)

       (*pack-with
         (lambda (sign rest)
            (lambda (first)
              `(,(string->symbol (string sign)) ,first ,rest)
            )))

      *star
      (*caten 2)
        (*pack-with (lambda (first lst)
              (fold-left (lambda (op elem)
                              (elem op)) first lst)
            ))
      done))

      (define <InfixArgList>
        (new
          (*delayed (lambda () <InfixExpression>))

          (*parser (word ","))
          (*delayed (lambda () <InfixExpression>))
          (*caten 2)
          (*pack-with (lambda (comma exp)
                        exp))
          *star
          (*caten 2)

          ;(*parser (char #\949)) <--- epsilon
          ;(*disj 2)
          done))

      (define <InfixMiddle>
        (new
          (*delayed (lambda () <InfixFuncall>))
          (*delayed (lambda () <InfixArrayGet>))
          (*disj 2)
          done))

      (define <InfixArrayGet>
        (new
          (*delayed (lambda () <End>))

          (*parser (char #\[))
          (*delayed (lambda () <InfixExpression>))
          (*parser (char #\]))
          (*caten 3)
          (*pack-with (lambda (open pos close)
                        (lambda (arr)
                          `(vector-ref ,arr ,pos))
                          ))
          *star
          (*caten 2)
          (*pack-with (lambda (arr lst)
                        (fold-left (lambda (a func)
                                            (func a)) arr lst)
                        ))

          done))

      (define <InfixFuncall>
        (new
          (*delayed (lambda () <End>))

          (*parser (char #\())
          (*parser <InfixArgList>)
          (*parser (char #\)))
          (*caten 3)
          (*pack-with (lambda (open args close)
                          (lambda (func)
                          `(,func ,@(fold-left cons (car args) (cdr args)))
                            )))
          (*caten 2)
          (*pack-with (lambda (func lambda)
                        (lambda func)))
          done))


      (define <InfixParen>
        (new
          (*parser (char #\())
          (*delayed (lambda () <InfixExpression>))
          (*parser (char #\)))
          (*caten 3)
          (*pack-with (lambda (open exp close)
            exp
              ))
          done))

      (define <InfixSexprEscape>
        (new
          (*delayed <InfixPrefixExtenstionPrefix>)
          (*delayed (lambda () <Sexpr>))
          (*caten 2)

          done))

  (define <End>
    (new
      (*parser <Number>)
      (*parser <InfixSymbol>)
      (*parser <InfixParen>)
      (*disj 3)
      done))



  (define <InfixExpression>
    (new
      (*parser <InfixAddSub>)
    done))

  (define <InfixExtention>
    (new
      (*parser <InfixPrefixExtenstionPrefix>)
      (*parser <InfixExpression>)
      (*caten 2)
      (*pack-with (lambda (x y) y ))
      done))

(define <Sexpr>
  (new
      (*parser <Boolean>)
      (*parser <Char>)
      (*parser <Number>)
      (*parser <String>)
      (*parser <Symbol>)
      (*parser <ProperList>)
      (*parser <ImproperList>)
      (*parser <Vector>)
      (*parser <Quoted>)
      (*parser <QuasiQuoted>)
      (*parser <Unquoted>)
      (*parser <UnquotedAndSpliced>)
      (*parser <InfixExtention>)
      (*disj 13)
   done))

