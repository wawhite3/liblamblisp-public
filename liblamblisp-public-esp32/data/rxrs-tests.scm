
(news ";;;4.1.1 Variable references\n")
;;;4.1.1 Variable references
(define x 28)
x
(news ";;;4.1.2 Literal expressions\n")
;;;4.1.2 Literal expressions
(quote a)
(quote #(a b c))
(quote (+ 1 2))
'a
'#(a b c)
'(+ 1 2)
'(quote a)
''a
'"abc"
"abc"
'145932
145932
#|
DEBT
'#	;#
#	;#
'#(a 10)	;#(a 10)
#(a 10)	;#(a 10)
'#u8(64 65)	;#u8(64 65)
#u8(64 65)	;#u8(64 65)
|#
'#t
#t

(news ";;;4.1.3 Procedure calls\n")
;;;4.1.3 Procedure calls
(+ 3 4)
((if #f + *) 3 4)

(news ";;;4.1.4 Procedures\n")
;;;4.1.4 Procedures
((lambda (x) (+ x x)) 4)
(define reverse-subtract
  (lambda (x y) (- y x)))
(reverse-subtract 7 10)
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(add4 6)
((lambda x x) 3 4 4 6)
((lambda (x y . z) z) 3 4 5 6)

(news ";;;4.1.5 Conditionals\n")
;;;4.1.5 Conditionals
(if (> 3 2) 'yes 'no)
(if (> 2 3) 'yes 'no)
(if (> 3 2) (- 3 2) (+ 3 2))

(news ";;;4.1.6 Assignments\n")
;;;4.1.6 Assignments
(define x 2)
(+ x 1)
(set! x 4)
(+ x 1)

(news ";;;4.1.7 include\n")
;;;4.1.7 include
(include "dummy.scm")

(news ";;;4.2.1 Conditionals\n")
;;;4.2.1 Conditionals
(cond ((> 3 2) 'greater)
      ((< 3 2) 'less))
(cond ((> 3 2) 'greater)
      ((< 3 2) 'less)
      (else 'equal))
(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f))
(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))
(case (car '(c d))
  ((a) 'a)
  ((b) 'b))
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else => (lambda (x) x)))
(and (= 2 2) (> 2 1))
(and (= 2 2) (< 2 1))
(and 1 2 'c '(f g))
(and)
(or (= 2 2) (> 2 1))
(or (= 2 2) (< 2 1))
(or #f #f #f)
(or (memq 'b '(a b c))
    (/ 3 0))
(when (= 1 1.0)
  (display "1")
  (display "2"))
(unless (= 1 1.0)
  (display "1")
  (display "2"))

(news ";;;4.2.2 Binding constructs\n")
;;;4.2.2 Binding constructs
(let ((x 2) (y 3))
  (* x y))
(let ((x 2) (y 3))
  (let ((x 7)
	(z (+ x y)))
    (* z x)))
(let ((x 2) (y 3))
  (let* ((x 7)
	 (z (+ x y)))
    (* z x)))
(letrec ((even?
	  (lambda (n)
	    (if (zero? n)
		#t
		(odd? (- n 1)))))
	 (odd?
	  (lambda (n)
	    (if (zero? n)
		#f
		(even? (- n 1))))))
  (even? 88))
(letrec* ((p
	   (lambda (x)
	     (+ 1 (q (- x 1)))))
	  (q
	   (lambda (y)
	     (if (zero? y)
		 0
		 (+ 1 (p (- y 1))))))
	  (x (p 5))
	  (y x))
  y)

(news ";;;4.2.3 Sequencing\n")
;;;4.2.3 Sequencing
(define x 0)
(and (= x 0)
     (begin (set! x 5)
	    (+ x 1)))
(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))

(news ";;;4.2.4 Iteration\n")
;;;4.2.4 Iteration
(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i))
(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum)))
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

(news ";;;4.2.5 Delayed evaluation (commented out) pending completion\n")
;;;4.2.5 Delayed evaluation
#|
(force (delay (+ 1 2)))

(let ((p (delay (+ 1 2))))
  (list (force p) (force p)))
(define integers
  (letrec ((next
	    (lambda (n)
	      (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))
(head (tail (tail integers)))
(define (stream-filter p? s)
  (delay-force
   (if (null? (force s))
       (delay '())
       (let ((h (car (force s)))
	     (t (cdr (force s))))
	 (if (p? h)
	     (delay (cons h (stream-filter p? t)))
	     (stream-filter p? t))))))
(head (tail (tail (stream-filter odd? integers))))
(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
		(if (> count x)
		    count
		    (force p)))))
(define x 5)
(force p)
(begin (set! x 10)
       (force p))
(eqv? (delay 1) 1)
(pair? (delay (cons 1 2)))
(+ (delay (* 3 7)) 13)
(car
 (list (delay (* 3 7)) 13))
|#

(news ";;;4.2.6 Dynamic bindings - r7rs feature not supported\n")
;;;4.2.6 Dynamic bindings - r7rs feature not supported

(news ";;;4.2.7 Exception handling\n")
;;;4.2.7 Exception handling
(guard (condition
	((assq 'a condition) => cdr)
	((assq 'b condition)))
       (raise (list (cons 'a 42))))

(guard (condition
	((assq 'a condition) => cdr)
	((assq 'b condition)))
       (raise (list (cons 'b 23))))

(news ";;;4.2.8 Quasiquotation\n")
;;;4.2.8 Quasiquotation

`(list ,(+ 1 2) 4)
(let ((name 'a)) `(list ,name ',name))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)

;;;DEBT
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))
;;;end DEBT

`((1 2) 3 4 five 6)
(let ((a 3))
  (cons '(1 2)
	(cons a (cons 4 (cons 'five '(6))))))
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))
(let ((a 3)) (list (list 1 2) a 4 'five 6))
(quasiquote (list (unquote (+ 1 2)) 4))
'(quasiquote (list (unquote (+ 1 2)) 4))

(news ";;;4.2.9 Case-lambda r7rs feature not supported\n")
;;;4.2.9 Case-lambda r7rs feature not supported

(news ";;;4.3 Macros, use nlambda for macro functionality\n")
;;;4.3 Macros, initially use nlambda for macro functionality

(news ";;;5.3.1 Top level definitions\n")
;;;5.3.1 Top level definitions
(define add3 (lambda (x) (+ x 3)))
(add3 3)

(define first car)
(first '(1 2))

(news ";;;5.3.2 Internal definitions\n")
;;;5.3.2 Internal definitions
(let ((x 5))
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  (foo (+ x 3)))
(let ((x 5))
  (letrec* ((foo (lambda (y) (bar x y)))
	    (bar (lambda (a b) (+ (* a b) a))))
    (foo (+ x 3))))

(news ";;;5.3.3 Multiple values, r7rs feature not supported\n")
;;;5.3.3 Multiple values, r7rs feature not supported
(lamb-integrity-check)
(news ";;;5.4 Syntax definitions, use nlambda/define-syntax\n")
;;;5.4 Syntax definitions, use nlambda/define-syntax
#|(let ((x 1) (y 2))
  (define-syntax swap!
    (syntax-rules ()
      ((swap! a b)
       (let ((tmp a))
	 (set! a b)
	 (set! b tmp)))))
  (swap! x y)
  (list x y))
(lamb-integrity-check)
|#

(let ((x 1)
      (y 2)
      (swap! (nlambda (a b)
		      (let ((tmp a))
			(set! a b)
			set! b tmp)))
      )
  (swap! x y)
  (list x y)
  )

(news ";;;5.5 Record-type definitions TBD\n")
;;;5.5 Record-type definitions TBD
(news ";;;5.6 Libraries TBD\n")
;;;5.6 Libraries TBD
(news ";;;5.7 REPL \n")
;;;5.7 REPL 
(news ";;;6.1 Equivalence predicates\n")
;;;6.1 Equivalence predicates

(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (eqv? f g))
(eqv? 'a 'a)
(eqv? 'a 'b)
(eqv? 2 2)
(eqv? 2 2.0)
(eqv? '() '())
(eqv? 100000000 100000000)

(eqv? (cons 1 2) (cons 1 2))
(eqv? (lambda () 1)
      (lambda () 2))
(let ((p (lambda (x) x)))
  (eqv? p p))
(eqv? #f 'nil)
(eqv? "" "")
(eqv? '#() '#())
(eqv? (lambda (x) x)
      (lambda (x) x))
(eqv? (lambda (x) x)
      (lambda (y) y))
;;;(eqv? 1.0e0 1.0f0)
;;;(eqv? +nan.0 +nan.0)
(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(let ((g (gen-counter)))
  (eqv? g g))

(eqv? (gen-counter) (gen-counter))

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))

(let ((g (gen-loser)))
  (eqv? g g))

(eqv? (gen-loser) (gen-loser))

(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
	 (g (lambda () (if (eqv? f g) 'both 'g))))
  (eqv? f g))

(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (eqv? f g))

(eqv? '(a) '(a))
(eqv? "a" "a")
(eqv? '(b) (cdr '(a b)))
(let ((x '(a)))
  (eqv? x x))

(eq? 'a 'a)
(eq? '(a) '(a))
(eq? (list 'a) (list 'a))
(eq? "a" "a")
(eq? "" "")
(eq? '() '())
(eq? 2 2)
(eq? #\A #\A)
(eq? car car)

(let ((n (+ 2 3)))
  (eq? n n))
(let ((x '(a)))
  (eq? x x))
(let ((x '#()))
  (eq? x x))
(let ((p (lambda (x) x)))
  (eq? p p))

(equal? 'a 'a)
(equal? '(a) '(a))
(equal? '(a (b) c)
	'(a (b) c))
(equal? "abc" "abc")
(equal? 2 2)
(equal? (make-vector 5 'a)
	(make-vector 5 'a)) =⇒
(equal? '#1=(a b . #1#)
	'#2=(a b a b . #2#))=⇒
(equal? (lambda (x) x)
	(lambda (y) y))

(news ";;;6.2.6 Numerical operartions\n")
;;;6.2.6 Numerical operartions
(complex? 3+4i)
(complex? 3)
(real? 3)
(real? -2.5+0i)
(real? -2.5+0.0i)
(real? #e1e10)
(real? +inf.0)
(real? +nan.0)
(rational? -inf.0)
(rational? 3.5)
(rational? 6/10)
(rational? 6/3)
(integer? 3+0i)
(integer? 3.0)
(integer? 8/4)
(exact? 3.0)
(exact? #e3.0)
(inexact? 3.)
(exact-integer? 32)
(exact-integer? 32.0)
(exact-integer? 32/5)
(finite? 3)
(finite? +inf.0)
(finite? 3.0+inf.0i)
(infinite? 3)
(infinite? +inf.0)
(infinite? +nan.0)
(infinite? 3.0+inf.0i)
(nan? +nan.0)
(nan? 32)
(nan? +nan.0+5.0i)
(nan? 1+2i)
(max 3 4)
(max 3.9 4)
(+ 3 4)
(+ 3)
(+)
(* 4)
(*)
(- 3 4)
(- 3 4 5)
(- 3)
(/ 3 4 5)
(/ 3)
(abs -7)
(floor/ 5 2)
(floor/ -5 2)
(floor/ 5 -2)
(floor/ -5 -2)
(truncate/ 5 2)
(truncate/ -5 2)
(truncate/ 5 -2)
(truncate/ -5 -2)
(truncate/ -5.0 -2)
(gcd 32 -36)
(gcd)
(lcm 32 -36)
(lcm 32.0 -36)
(lcm)
(numerator (/ 6 4))
(denominator (/ 6 4))
(denominator
 (inexact (/ 6 4)))
(floor -4.3)
(ceiling -4.3)
(truncate -4.3)
(round -4.3)
(floor 3.5)
(ceiling 3.5)
(truncate 3.5)
(round 3.5)
(round 7/2)
(round 7)
(rationalize
 (exact .3) 1/10)
(rationalize .3 1/10)
(square 42)
(square 2.0)
(sqrt 9)
(sqrt -1)
(exact-integer-sqrt 4)
(exact-integer-sqrt 5)
(string->number "100")
(string->number "100" 16)
(string->number "1e2")
(news ";;;6.3 Booleans\n")
;;;6.3 Booleans
(not #t)
(not 3)
(not (list 3))
(not #f)
(not '())
(not (list))
(not 'nil)
(boolean? #f)
(boolean? 0)
(boolean? '())
(define x (list 'a 'b 'c))
(define y x)
y
(list? y)
(set-cdr! x 4)
x
(eqv? x y)
y
(list? y)
(set-cdr! x x)
(list? x)
(pair? '(a . b))
(pair? '(a b c))
(pair? '())
(pair? '#(a b))
(cons 'a '())
(cons '(a) '(b c d))
(cons "a" '(b c))
(cons 'a 3)
(cons '(a b) 'c)
(car '(a b c))
(car '((a) b c d))
(car '(1 . 2))
(car '())
(cdr '((a) b c d))
(cdr '(1 . 2))
(cdr '())
(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(set-car! (f) 3)
(set-car! (g) 3)

(list? '(a b c))
(list '())
(list? '(a . b))

(let ((x (list 'a)))
  (set-cdr! x x)
  (list? x))

(list 'a (+ 3 4) 'c)
(list)
(length '(a b c))
(length '(a (b) (c d e)))
(length '())
(append '(x) '(y))
(append '(a) '(b c d))
(append '(a (b)) '((c)))
(append '(a b) '(c . d))
(append '() 'a)
(reverse '(a b c))
(reverse '(a (b c) d (e (f))))
(list-ref '(a b c d) 2)

(list-ref '(a b c d)
	  (exact (round 1.8)))

(let ((ls (list 'one 'two 'five!)))
  (list-set! ls 2 'three)
  ls)

(list-set! '(0 1 2) 1 "oops")
(memq 'a '(a b c))
(memq 'b '(a b c))
(memq 'a '(b c d))
(memq (list 'a) '(b (a) c))

(member (list 'a)
	'(b (a) c))

(member "B"
	'("a" "b" "c")
	string-ci=?)

(memq 101 '(100 101 102))
(memv 101 '(100 101 102))

(define e '((a 1) (b 2) (c 3)))
(assq 'a e)
(assq 'b e)
(assq 'd e)
(assq (list 'a) '(((a)) ((b)) ((c))))
(assoc (list 'a) '(((a)) ((b)) ((c))))
(assoc 2.0 '((1 1) (2 4) (3 9)) =)
(assq 5 '((2 3) (5 7) (11 13)))
(assv 5 '((2 3) (5 7) (11 13)))
(define a '(1 8 2 8)) ; a may be immutable
(define b (list-copy a))
(set-car! b 3)
b
a

(news ";;;6.5 Symbols\n")
;;;6.5 Symbols
(symbol? 'foo)
(symbol? (car '(a b)))
(symbol? "bar")
(symbol? 'nil)
(symbol? '())
(symbol? #f)
(symbol->string 'flying-fish)
(symbol->string 'Martin)
(symbol->string
 (string->symbol "Malvina"))
(string->symbol "mISSISSIppi")
(eqv? 'bitBlt (string->symbol "bitBlt"))
(eqv? 'LollyPop
      (string->symbol
       (symbol->string 'LollyPop)))
(string=? "K. Harper, M.D."
	  (symbol->string
	   (string->symbol "K. Harper, M.D.")))

(news ";;;6.6 Characters\n")
;;;6.6 Characters
(digit-value #\3)
(digit-value #\x0664)
(digit-value #\x0AE6)
(digit-value #\x0EA6)

(news ";;;6.7 Strings\n")
;;;6.7 Strings
"The word \"recursion\" has many meanings."
"Another example:\ntwo lines of text"
"Here's text \
containing just one line"
"\x03B1; is named GREEK SMALL LETTER ALPHA."
(define (f) (make-string 3 #\*))
(define (g) "***")
(string-set! (f) 0 #\?)
(string-set! (g) 0 #\?)
(string-set! (symbol->string 'immutable)
	     0
	     #\?)
(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 1 a 0 2)

(news ";;;6.8 Vectors\n")
;;;6.8 Vectors
(vector 'a 'b 'c)
(vector-ref '#(1 1 2 3 5 8 13 21)
	    5)
(vector-ref '#(1 1 2 3 5 8 13 21)
	    (exact
	     (round (* 2 (acos -1)))))
(let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec)
(vector-set! '#(0 1 2) 1 "doe")
(vector->list '#(dah dah didah))
(vector->list '#(dah dah didah) 1 2)
(list->vector '(dididit dah))
(string->vector "ABC")
(vector->string
 #(#\1 #\2 #\3))
(define a #(1 8 2 8))
(define b (vector-copy a))
(vector-set! b 0 3)
(define c (vector-copy b 1 3))
(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
b
(vector-append #(a b c) #(d e f))
(define a (vector 1 2 3 4 5))
(vector-fill! a 'smash 2 4)
a

(news ";;;6.9 Bytevectors\n")
;;;6.9 Bytevectors
#u8(0 10 5)
(make-bytevector 2 12)
(bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21)
		   5)
(let ((bv (bytevector 1 2 3 4)))
  (bytevector-u8-set! bv 1 3)
  bv)
(define a #u8(1 2 3 4 5))
(bytevector-copy a 2 4))
(define a (bytevector 1 2 3 4 5))
(define b (bytevector 10 20 30 40 50))
(bytevector-copy! b 1 a 0 2)
b
(bytevector-append #u8(0 1 2) #u8(3 4 5))
(utf8->string #u8(#x41))
(string->utf8 "λ")

(news ";;;6.10 Control features\n")
;;;6.10 Control features
(procedure? car)
(procedure? 'car)
(procedure? (lambda (x) (* x x)))
(procedure? '(lambda (x) (* x x)))
(news ";;;(call-with-current-continuation procedure?)\n")
;;;(call-with-current-continuation procedure?)
(apply + (list 3 4))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

((compose sqrt *) 12 75)
(map cadr '((a b) (d e) (g h)))
(map (lambda (n) (expt n n))
'(1 2 3 4 5))

(let ((count 0))
  (map (lambda (ignored)
	 (set! count (+ count 1))
	 count)
       '(a b)))

(string-map char-foldcase "AbdEgH")

(string-map
(lambda (c)
  (integer->char (+ 1 (char->integer c))))
"HAL")

(string-map
 (lambda (c k)
   ((if (eqv? k #\u) char-upcase char-downcase)
    c))
 "studlycaps xxx"
 "ululululul")

(vector-map cadr '#((a b) (d e) (g h)))
(vector-map (lambda (n) (expt n n)) '#(1 2 3 4 5))
(vector-map + '#(1 2 3) '#(4 5 6 7))

(let ((count 0))
  (vector-map
   (lambda (ignored)
     (set! count (+ count 1))
     count)
   '#(a b)))

(let ((v (make-vector 5)))
  (for-each (lambda (i)
	      (vector-set! v i (* i i)))
	    '(0 1 2 3 4)))

(let ((v '()))
  (string-for-each
   (lambda (c) (set! v (cons (char->integer c) v)))
   "abcde")
  v)

(let ((v (make-list 5)))
  (vector-for-each
   (lambda (i) (list-set! v i (* i i)))
   '#(0 1 2 3 4))
  v)
