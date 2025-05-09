(news "debug tests executing now\n")
'foo
(define (tail-sum accum next)
  (if (= next 0)
      accum
      (tail-sum (+ accum next) (- next 1))
      )
  )

		      
(define (tsum x)
  (tail-sum 0 x)
  )

(tsum 9)
(tsum 250)
(tsum 1000)

(news "Should see #(0 1 2 3 4)\n")
(make-vector 5)
(define vec (make-vector 5))
(vector-set! vec 0 10)
(vector-set! vec 1 11)
(vector-set! vec 2 12)
(vector-set! vec 3 13)
(vector-set! vec 4 14)

(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i))
(vector->list vec)

(news "Should see 25\n")
(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum)))

(news "Should see 25\n")
(do ((x '(1 3 5 7 9) (cdr x))
     (sum 0 (+ sum (car x)))
     )
    ((null? x) sum)
  )

;;;(quote #(3 0x3fcf402c))
(vector 0 (quote (2 2 2 2)) "Anna")
(vector->list (vector 0 (quote (2 2 2 2)) "Anna"))
(let ((vec (vector 0 (quote (2 2 2 2)) "Anna"))
      )
  (vector-set! vec 1 (quote ("Sue" "Sue")))
  (vector->list vec)
  )

#|
(define (ev x)
  (eval (qq-expand-toplevel x)
        (interaction-environment)))

(define (qq-expand-toplevel x)
  (if (eq? 'quasiquote (car x))
      (qq-expand (cadr x) 0)
      x))

(define (qq-expand x depth)
  (if (pair? x)
      (case (car x)
        ((quasiquote)
         `(cons ',(car x) ,(qq-expand (cdr x) (+ depth 1))))
        ((unquote unquote-splicing)
         (cond ((> depth 0)
                `(cons ',(car x) ,(qq-expand (cdr x) (- depth 1))))
               ((and (eq? 'unquote (car x))
                     (not (null? (cdr x)))
                     (null? (cddr x)))
                (cadr x))
               (else 
                (error "Illegal"))))
        (else
         `(append ,(qq-expand-list (car x) depth)
                  ,(qq-expand (cdr x) depth))))
      `',x))

(define (qq-expand-list x depth)
  (if (pair? x)
      (case (car x)
        ((quasiquote)
         `(list (cons ',(car x) ,(qq-expand (cdr x) (+ depth 1)))))
        ((unquote unquote-splicing)
         (cond ((> depth 0)
                `(list (cons ',(car x) ,(qq-expand (cdr x) (- depth 1)))))
               ((eq? 'unquote (car x))
                `(list . ,(cdr x)))
               (else
                `(append . ,(cdr x)))))
        (else
         `(list (append ,(qq-expand-list (car x) depth)
                        ,(qq-expand (cdr x) depth)))))
      `'(,x)))
|#
	    
(term "Look for failure\n")
(define a 1)
(define b 2)
(define c 3)
(define d 4)
(define e 5)
(define f 6)
#|
`a
`,a
`(foo ,a ,b)
`(foo + 1 2)
`(foo (+ 1 2))
`(foo ,(+ 1 2))
`(foo ,(+ a b))
`(foo ,a `(foo + 1 2) ,b)
`(foo ,a `(foo ,(+ 1 2)) ,b)

(define l1 '(a b c d e f))
(define l2 '(x y z w))

(quasiquote (b (unquote (+ 1 2))))

`l1
`,l1
`(foo ,l1)
`(foo ,@l1)

(term "Should see ((1 2) 3 4 five 6)\n")
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))

(term "Should see (list 3 4)\n")
(quasiquote (list (unquote (+ 1 2)) 4))
|#

#|
(term "Should see (a `(b (+ 1 2) ,(foo 4 d) e) f)\n")
`(a `(b (+ 1 2) ,(foo ,(+ 1 3) d) e) f)

(term "Should see (a `(b ,(+ 1 2) (foo ,(+ 1 3) d) e) f)\n")
`(a `(b ,(+ 1 2) (foo ,(+ 1 3) d) e) f)

(term "Should see (a `(b ,(+ 1 2) ,(foo (+ 1 3) d) e) f)\n")
`(E
a `(b ,(+ 1 2) ,(foo (+ 1 3) d) e) f)

;(quasiquote (b (unquote (+ 1 2)) (unquote (foo (unquote (+ 1 3)) d)) e))
;;(quasiquote (a (quasiquote (b (unquote (+ 1 2)) (unquote (foo (unquote (+ 1 3)) d)) e)) f))
(term "No failure\n")

`(list ,(+ 1 2) 4)
(let ((name 'a)) `(list ,name ',name))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))

(term "Fails here\n")
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
(term "Did not fail\n")
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))
`((1 2) 3 4 five 6)
(let ((a 3))
  (cons '(1 2)
	(cons a (cons 4 (cons 'five '(6))))))
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))
(let ((a 3)) (list (list 1 2) a 4 'five 6))
(quasiquote (list (unquote (+ 1 2)) 4))
'(quasiquote (list (unquote (+ 1 2)) 4))

|#
(let loopn ((numbers '(3 -2 1 6 -5))
	   (nonneg '())
	   (neg '()))
  (cond ((null? numbers) (list nonneg neg))
	((>= (car numbers) 0)
	 (loopn (cdr numbers)
	       (cons (car numbers) nonneg)
	       neg))
	((< (car numbers) 0)
	 (loopn (cdr numbers)
	       nonneg
	       (cons (car numbers) neg)))))


1
'1
'(1)
'(1 . 3)
'(5 . (99 72))
'(5 . (99 . 72))
(+ 1 1)
(+ (* 5 2) (* 3 4))
'(1 2 3)
if

;;;(if)
'(if 1 2 3)
(if 4 5 6)
(if (if 1 4 6) 8)
(if #t 1)
(if #t 1 2)
(if #f 1)
(if #f 1 2)
(if #f #f #t)
'(1 2 3)
'(3 44 55 66 77 88)
'(if ( then (else) if))

(let ((name 'b)) name)

(expt 2.0 4)
(news "Testing map\n")
(map + '(1 2 3) '(4 5 6))
(map cadr '((a b) (d e) (g h)))
(map (lambda (n) (expt n n)) '(1 2 3 4 5))

(let ((count 0))
  (map (lambda (ignored)
	 (set! count (+ count 1))
	 count)
       '(a b)))
(news "map test finished\n")

#|
(news "Testing QQ\n")
`(list ,(+ 1 2) 4)
`(1 2 3 ,(list 4 5))
`(1 2 3 ,@(list 4 5))

(let ((name 'a)) `(list ,name))
(let ((name 'a)) `(list ',name))
(let ((name 'a)) `(list ,name ',name))

`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)

`(,@(cdr '(c)) . ,(car '(cons)))
`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))


`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))

`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)

(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))

`((1 2) 3 4 five 6)
(let ((a 3))
  (cons '(1 2)
	 (cons a (cons 4 (cons 'five '(6))))))
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))
(let ((a 3)) (list (list 1 2) a 4 'five 6))
(quasiquote (list (unquote (+ 1 2)) 4))
(quasiquote (list (unquote (+ 1 2)) 4))
(news "Done Testing QQ\n")

(news "loopx test starting\n")
(define st (micros))

(let loopx2 ((x 10))
  (if (eqv? x 0)
      (begin
	(term "x is zero\n")
	#t
	)
      (begin
	(term "x is ~a\n" x)
	(loopx2 (- x 1))
	)
      )
  )

(define elapsed (- (micros) st))
(news "loopx test finished elapsed == ~aus\n" elapsed)
|#

(define (variadic . args)
  (printf "simple variadic1 test happening now\n")
  (printf "simple variadic1 test args == ~a\n" args)
  (printf "simple variadic1 test done\n")
  )

(variadic)
(variadic 1)
(variadic 1 2)
(variadic 3 4 5)
(variadic '(1 2) '(3 4))
#|
`(list ,(+ 1 2) 4)

(let ((name 'a))
  `(list ,name ',name)
  )

(news "qq test done\n")
|#
(news "New section\n")

(define foo 678)
foo
(define bar (lambda(x) x))
bar
(bar 101)
(define (barbar x y) x y)
barbar
(barbar 23 22)
(define (sub1 x) (- x 1))
(define (addx x) (+ x))
addx
(define (addxy x y) (+ x y))
addxy
(define (addxyz x y z) (+ x y z))
addxyz
(addxyz 7 8 9)
(addxyz 10 (+ 34 55) 11)
(addxyz 12 (* 4 5) (+ 7 8))
(define testtesttest 5)
testtesttest
(define (test) 6)
test
(test)
(define (test n) n)
test
(test 4)
(define (recbasic n) n)
recbasic
(recbasic 8)
(define (recbasic n) (if (> n 1) (+ n 1) n))
(recbasic 2)
freestack
(freestack)
(news "End new section\n")

(symbol-table-analyze (current-symbol-table) 1)
(define (factrecurse x)
  (if (eq? x 1)
      1
      (* x (factrecurse (- x 1)))
      ))
(factrecurse 5)
(news "factrecurse finished\n")

(symbol-table-analyze (current-symbol-table) 1)
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)
      ))
(symbol-table-analyze (current-symbol-table) 1)
(define (fact n) (fact-iter 1.0 1 n))

(symbol-table-analyze (current-symbol-table) 1)
(news "(fact 1)\n")
(fact 1)
(news "(tsum 10)\n")
(tsum 10)
(symbol-table-analyze (current-symbol-table) 1)
(news "(fact 2)\n")
(fact 2)
(symbol-table-analyze (current-symbol-table) 1)
(news "(fact 3)\n")
(fact 3)
(symbol-table-analyze (current-symbol-table) 1)
(news "(fact 4)\n")
(fact 4)
(news "(fact 5)\n")
(fact 5)
(news "(fact 1000)\n")
(fact 1000)

(news "Defining fib\n")

(symbol-table-analyze (current-symbol-table) 1)
(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1))
             (fib (- n 2))))))
(symbol-table-analyze (current-symbol-table) 1)
(news "fib defined\n")

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

(define (fib-iter a b count)
  (if (eq? count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(define (fib2 n) (fib-iter 1 0.0 n))  
(fib2 0)
(fib2 1)
(fib2 2)
(fib2 3)
(fib2 4)
(fib2 5)
(fib2 6)
(fib2 7)
(fib2 8)
(fib2 9)
(fib2 45)
(fib2 46)
(fib2 47)
(fib2 48)
(fib2 49)
(fib2 50)

(let () 0)
(let ((n "SUCCESS")) n)

(define (tail-sum accum next)
  (if (= next 0)
      accum
      (tail-sum (+ accum next) (- next 1))
      )
  )

(define (tsum x)
  (tail-sum 0 x)
  )

(tsum 9)
(tsum 250)
(tsum 1000)
(tsum 10000)

(define (fact-iter accum next)
  (if (= accum 0)
      'likely-overflow
      (if (= next 1)
	  accum
	  (fact-iter (* accum next) (- next 1))
	  )
      )
  )

(define (fact x) (fact-iter 1 x))

(fact 3)
(fact 6)
(fact 9)
(fact 200.0)

(news "================ apply test\n")

(apply + '(1))
(apply + '(1 2))
(apply + 1 2 '())
(apply + 1 2 '(3 4))
(apply + '(1 2 3))
(apply + 1 2 3 '(4 5 6))
(apply + (list 3 4))
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args ))))
  )

((compose sqrt *) 12 75)

(begin 1 2 3)

(if LambLisp? (news "current env is ~a\n" (current-environment)))

(define (variadic . args)
  (printf "simple variadic args == ~a\n" args)
  )

(variadic)
(variadic 1)
(variadic 1 2)
(variadic 3 4 5)
(variadic '(1 2) '(3 4))

(define (variadic first . rest)
  (printf "first ~a rest ~a\n" first rest)
  )
(variadic 1)
(variadic 1 2)
(variadic 3 4 5)
(variadic '(1 2) '(3 4))

(define (variadic first second . rest)
  (printf "first ~a second ~a rest ~a\n" first second rest)
  )
(variadic 1 2)
(variadic 3 4 5)
(variadic '(1 2) '(3 4))

(define (variadic first second third . rest)
  (printf "first ~a second ~a third ~a rest ~a\n" first second third rest)
  )
(variadic 3 4 5 6 7 8)

(printf "~a\n" (format "test ~a ~a ~a" "yes a test," " a very good test, number " 1))

(news "end of variadic tests\n")
(news "apply test\n")

(apply + '(1))
(apply + '(1 2))
(apply + 1 2 '(3 4))
(apply + '(1 2 3))
(apply + 1 2 3 '(4 5 6))

(term "let test\n")
(let () 0)
(let ((n "succes")) n)
(let ((x 1)
      (y 3)
      )
  (printf "x == ~a\n" x)
  (printf "x+1 == ~a\n" (+ 1 x))
  (printf "list? == ~a\n" (list? 1))
  )

(let  ((x 2) (y 3)) (* x y))
(let* ((x 2) (y 3)) (* x y))

(let ((x 2) (y 3))
  (let ((x 7)
	(z (+ x y)))
    (* z x)))

(term "============= let* test - result 70\n")

(let ((x 2) (y 3))
  (let* ((x 7)
	 (z (+ x y)))
    (* z x)))


(if LambLisp?
    (begin
      (list-analyze 1)
      (list-analyze (+ 1 2))
      (list-analyze '(a b c))
      (list-analyze '(a b c . d))
      )
    )

(list? 1)
(list? (+ 1 2))
(list? '(a b c))

(+ 3 4)
(* 3 4)
(- 3 4)
(/ 3 4)
(/ 3.0 4)
(/ 3 4.0)

(define (ntest a b c d)
  (write a)
  (write b)
  (write c)
  (write d)
  (printf "\n")
  )



(warn "============ letrec test \n")

((lambda (n)
   (if (zero? n)
       #t
       (- n 1)
       ))
 1086)

(zero? 0)
(if (zero? 0) #t #f)
(if (odd? 0) #t #f)
(if (even? 0) #t #f)

((lambda (n) (if (zero? n) #t n)) 42)
((lambda (n) (if (zero? n) #t n)) 42.0)
((lambda (n) (if (zero? n) #t n)) 0)
((lambda (n) (if (zero? n) #t n)) 0.0)

(define foo '(3 -2 1 6 -5))
foo
(define foo '(numbers (3 -2 1 6 -5)))


(news "loopx test starting\n")

(define st (micros))
(let loopx ((x 100))
  (if (eqv? x 0)
      #t
      (loopx (- x 1))
      )
  )

(define elapsed (- (micros) st))
(news "loopx test finished\n")

(news "elapsed == ~a us\n" elapsed)

(term "complex loopx test starting\n")
(let loopx ((numbers (quote (3 -2 1 6 -5)))
	   (nonneg '())
	   (neg '())
	   )
  (cond ((null? numbers) (list nonneg neg))
	((>= (car numbers) 0)
	 (loopx (cdr numbers)
	       (cons (car numbers) nonneg)
	       neg))
	((< (car numbers) 0)
	 (loopx (cdr numbers)
	       nonneg
	       (cons (car numbers) neg)))))
(term "Should see ((6 1 3) (-5 -2)) just above\n")

(warn "letrec acid test\n")

(define st (micros))
(letrec ((is_even? (lambda (n)
		     (if (zero? n)
			 #t
			 (is_odd? (- n 1))
			 )))
	 (is_odd? (lambda (n)
		    (if (zero? n)
			#f
			(is_even?(- n 1))
			)))
	 )
  (is_even? 10000)
  )

(define elapsed (- (micros) st))
(news "elapsed == ~a us\n" elapsed)


(term "============= letrec test finished\n")

					;(test name expect expr)
					;OR
					;(test expect expr)
(define (test2 expect expr)
  (if (equal? expect expr)
      (news "PASS\n")
      (errmsg "FAIL\n")
      )
  )

(test2 #t #t)
(test2 #t #f)

(define (test . args)
  (if (eqv? 3 (length args))
      (test2 (cadr args) (caddr args))
      (let ((kar (car args))
	    (kadr (cadr args)))
	(test2 kar kadr)
	)
      )
  )

(test #t #t)
(test "foo" #t #t)

(let ((me "let-test1()")
      (x 5)
      (y 8)
      )
  (+ x y)
  #t
  )
(term "should see a number\n")


(test #t
      (let ((me "let-test2()"))
	(warn "~a happening now 10\n" me)
	(warn "~a happening now 20 env == ~a\n" me (interaction-environment))
	#t
	)
      )

(test #t
      (let ((me "let-test3()"))
	(warn "~a happening now 10\n" me)
	#t
	(warn "~a happening now 20\n" me)
	(+ 1 2)
	)
      )

(term "finished let-test\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(news "qq test\n")

(test '(list a 'a)
      (let ((name 'a))
	`(list ,name ',name)
	)
      )
(news "qq test done\n")

(define (foo x) (+ x x))
foo
(test 8 (foo 4))

(define foo (lambda (x) (+ x x)))
foo

(test 8 ((lambda (x) (+ x x)) 4))

(test '(3 4 5 6) ((lambda x x) 3 4 5 6))

(test '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(test 'yes (if (> 3 2) 'yes 'no))

(test 'no (if (> 2 3) 'yes 'no))

(test 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(test 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))

(test 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))

(test 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))

(test 'consonant
      (case (car '(c d))
	((a e i o u) 'vowel)
	((w y) 'semivowel)
	(else 'consonant)))

(test #t (and (= 2 2) (> 2 1)))

(test #f (and (= 2 2) (< 2 1)))

(test '(f g) (and 1 2 'c '(f g)))

(test #t (and))

(test #t (or (= 2 2) (> 2 1)))

(test #t (or (= 2 2) (< 2 1)))

(test '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test 6 (let ((x 2) (y 3)) (* x y)))

(test 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

(test 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))

(test -2 (let ()
           (define x 2)
           (define f (lambda () (- x)))
           (f)))

(define let*-def 1)
(let* () (define let*-def 2) #f)
(test 1 let*-def)

(test '#(0 1 2 3 4)
      (do ((vec (make-vector 5))
	   (i 0 (+ i 1)))
	  ((= i 5) vec)
	(vector-set! vec i i)))

(test 25
      (let ((x '(1 3 5 7 9)))
	(do ((x x (cdr x))
             (sum 0 (+ sum (car x))))
            ((null? x)
             sum))))
(warn "10 here is a warning\n")

(test '((6 1 3) (-5 -2))
      (let loopx ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '())
		 )
	(cond
	 ((null? numbers) (list nonneg neg))
	 ((>= (car numbers) 0) (loopx (cdr numbers) (cons (car numbers) nonneg) neg))
	 ((< (car numbers) 0)  (loopx (cdr numbers) nonneg (cons (car numbers) neg)))
	 )
	)
      )

(warn "20 here is a warning\n")

(test '(list 3 4) `(list ,(+ 1 2) 4))

(test '(list a 'a)
      (let ((name 'a))
	`(list ,name ',name)
	)
      )

(test '(a 3 4 5 6 b)
      `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

(test '(10 5 4 16 9 8)
      `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n 2)) '(4 3)) 8))

(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(test '(a `(b ,x ,'y d) e)
      (let ((name1 'x)
            (name2 'y))
	`(a `(b ,,name1 ,',name2 d) e)))


(test '(list 3 4)
      (quasiquote (list (unquote (+ 1 2)) 4)))

(test #t (eqv? 'a 'a))

(test #f (eqv? 'a 'b))

(test #t (eqv? '() '()))

(test #f (eqv? (cons 1 2) (cons 1 2)))

(test #f (eqv? (lambda () 1) (lambda () 2)))

(test #t (let ((p (lambda (x) x))) (eqv? p p)))

(test #t (eq? 'a 'a))

(test #f (eq? (list 'a) (list 'a)))

(test #t (eq? '() '()))

(test #t (eq? car car))

(test #t (let ((x '(a))) (eq? x x)))

(test #t (let ((p (lambda (x) x))) (eq? p p)))

(test #t (equal? 'a 'a))

(test #t (equal? '(a) '(a)))

(test #t (equal? '(a (b) c) '(a (b) c)))

(test #t (equal? "abc" "abc"))

(test #f (equal? "abc" "abcd"))

(test #f (equal? "a" "b"))

(test #t (equal? 2 2))

(test #f (eqv? 2 2.0))

(test #f (equal? 2.0 2))

(test #t (equal? (make-vector 5 'a) (make-vector 5 'a)))

(test 4 (max 3 4))

(test 4 (max 3.9 4))

(test 7 (+ 3 4))

(test 3 (+ 3))

(test 0 (+))

(test 4 (* 4))

(test 1 (*))

(test -1 (- 3 4))

(test -6 (- 3 4 5))

(test -3 (- 3))

(test -1.0 (- 3.0 4))

(test 7 (abs -7))

(test 1 (modulo 13 4))

(test 1 (remainder 13 4))

(test 3 (modulo -13 4))

(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))

(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))

(test -1 (remainder -13 -4))

(test 4 (gcd 32 -36))

(test 288 (lcm 32 -36))

(test 100 (string->number "100"))

(test 256 (string->number "100" 16))

(test 127 (string->number "177" 8))

(test 5 (string->number "101" 2))

(test 100.0 (string->number "1e2"))

(test "100" (number->string 100))

(test "100" (number->string 256 16))

(test "ff" (number->string 255 16))

(test "177" (number->string 127 8))

(test "101" (number->string 5 2))

(test #f (not 3))

(test #f (not (list 3)))

(test #f (not '()))

(test #f (not (list)))

(test #f (not '()))

(test #f (boolean? 0))

(test #f (boolean? '()))

(test #t (pair? '(a . b)))

(test #t (pair? '(a b c)))

(test '(a) (cons 'a '()))

(test '((a) b c d) (cons '(a) '(b c d)))

(test '("a" b c) (cons "a" '(b c)))

(test '(a . 3) (cons 'a 3))

(test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

(test 2 (cdr '(1 . 2)))

(test #t (list? '(a b c)))

(test #t (list? '()))

(test #f (list? '(a . b)))

(test #f
      (let ((x (list 'a)))
	(set-cdr! x x)
	(list? x)
	)
      )

(test '(a 7 c) (list 'a (+ 3 4) 'c))

(test '() (list))

(test 3 (length '(a b c)))

(test 3 (length '(a (b) (c d e))))

(test 0 (length '()))

(test '(x y) (append '(x) '(y)))

(test '(a b c d) (append '(a) '(b c d)))

(test '(a (b) (c)) (append '(a (b)) '((c))))

(test '(a b c . d) (append '(a b) '(c . d)))

(test 'a (append '() 'a))

(test '(c b a) (reverse '(a b c)))

(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(test 'c (list-ref '(a b c d) 2))

(test '(a b c) (memq 'a '(a b c)))

(test '(b c) (memq 'b '(a b c)))

(test #f (memq 'a '(b c d)))

(test #f (memq (list 'a) '(b (a) c)))

(test '((a) c) (member (list 'a) '(b (a) c)))

(test '(101 102) (memv 101 '(100 101 102)))

(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))

(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test #t (symbol? 'foo))

(test #t (symbol? (car '(a b))))

(test #f (symbol? "bar"))

(test #t (symbol? 'nil))

(test #f (symbol? '()))

(test "flying-fish" (symbol->string 'flying-fish))

(test "Martin" (symbol->string 'Martin))

(test "Malvina" (symbol->string (string->symbol "Malvina")))

(test #t (string? "a"))

(test #f (string? 'a))

(test 0 (string-length ""))

(test 3 (string-length "abc"))


(test #t (string<? "a" "aa"))

(test #f (string<? "aa" "a"))

(test #f (string<? "a" "a"))

(test #t (string<=? "a" "aa"))

(test #t (string<=? "a" "a"))

(test "" (substring "abc" 0 0))

(test "a" (substring "abc" 0 1))

(test "bc" (substring "abc" 1 3))

(test "abc" (string-append "abc" ""))

(test "abc" (string-append "" "abc"))

(test "abc" (string-append "a" "bc"))

(test '#(0 ("Sue" "Sue") "Anna")
      (let ((vec (vector 0 '(2 2 2 2) "Anna")))
	(vector-set! vec 1 '("Sue" "Sue"))
	vec))

(test '(dah dah didah) (vector->list '#(dah dah didah)))

(test '#(dididit dah) (list->vector '(dididit dah)))

(test #t (procedure? car))

(test #f (procedure? 'car))

(test #t (procedure? (lambda (x) (* x x))))

(test #f (procedure? '(lambda (x) (* x x))))

(test 7 (apply + (list 3 4)))

(test '(b e h) (map cadr '((a b) (d e) (g h))))

(test '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

(test '(5 7 9) (map + '(1 2 3) '(4 5 6)))

(test '#(0 1 4 9 16)
      (let ((v (make-vector 5)))
	(for-each
	 (lambda (i) (vector-set! v i (* i i)))
	 '(0 1 2 3 4))
	v))

(test 3 (force (delay (+ 1 2))))

(test '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))

(test 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))

;(test 'ok (let ((=> 1)) (cond (#t => 'ok))))

(test '(,foo) (let ((unquote 1)) `(,foo)))

(test '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))
#|
(test 'ok
      (let ((... 2))
	(let-syntax ((s (syntax-rules ()
                          ((_ x ...) 'bad)
                          ((_ . r) 'ok))))
          (s a b c))))

(test 'ok (let ()
            (let-syntax ()
              (define internal-def 'ok))
            internal-def))

(test 'ok (let ()
            (letrec-syntax ()
              (define internal-def 'ok))
            internal-def))
|#
(test '(2 1)
      ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (list x y))))))

(test '(2 2)
      ((lambda () (let ((x 1)) (set! x 2) (let ((y x)) (list x y))))))

(test '(1 2)
      ((lambda () (let ((x 1)) (let ((y x)) (set! y 2) (list x y))))))

(test '(2 3)
      ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (set! y 3) (list x y))))))

(define f
  (let ((a 5))
    (lambda (m) 
      (if (> m 7)
          (set! a (- a 1))
          (set! a (+ a m)))
      a)))
(f 1)
(f 2)

					;gives different answers on chez vs tiny, I think because of arg eval order
(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ 1 n))
      n)))
(list (counter) (counter) (counter))

(define (make-counter n)
  (lambda ()
    (set! n (+ 1 n))
    n))
(define a (make-counter 0))
(define b (make-counter 10))
(list (a) (a) (a) (b) (b) (a) (a))

