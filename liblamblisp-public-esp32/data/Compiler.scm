
(define (compile-list args env)
  (letrec ((f (lambda (args env res)
	     (if (null? args) res
		 (f (cdr args) env (cons (compile (car args) env) res))
		 )
	     )
	   )
	)
    (f args env nil)
    )
  )

(define (compile-pair sexpr env)
  (let* ((me "(compile-pair)")
	 (proc (car sexpr))
	 (args (cdr sexpr))
	 )
    (cond ((pair? proc)   (set! proc (compile-pair proc env)))
	  ((symbol? proc) (set! proc (compile-symbol proc env)))
	  )
    
    (cond ((lamb-mop3-proc? proc)  (cons proc (compile-list args env)))
	  ((lamb-mop3-nproc? proc) (cons proc (compile-mop3-nproc-args proc args env)))
	  ((procedure? proc)       (cons (compile-procedure  proc env) (compile-list args env)))
	  ((nprocedure? proc)      (cons (compile-nprocedure proc env) args))
	  (else sexpr)
	  )
    )
  )

(define (compile-condargs args env)
  (if (null? args) nil
      (let* ((clause (car args))
	     (compiled-clause
	      (cons (compile-sexpr (car clause ) env)
		    (compile-sexpr (cadr clause) env)
		    )
	      )
	     )
	(cons compiled-clause (compile-condargs (cdr args) env))	;DEBT make tail recursive
	)
      )
  )

(define (compile-lambda-args args env)
  (let ((formals (car args))
	(body (cdr args))
	(compile-env (lamb-env-add-frame formals formals env))
	)
    (cons formals (compile-list body compile-env))
  )

(define (compile-let-args proc args env)
  (let* ((bindings (car args))
	 (body (cdr args))
	 (symbols (map car bindings))
	 (compile-one-binding (lambda (b env)
				(cons (car b) (compile-sexpr (cadr b) env))
				)
			      )
	 (compile-bindings (lambda b result env)
			   (if (null? b) result
			       (compile-bindings (cdr b) (cons (compile-one-binding (car b) env) result))
			       )
			   )
	 (compiled-bindings (compile-bindings bindings nil env))
	 (compile-env (lamb-add-frame symbols symbols env))
	 )
    (cons compiled-bindings (compile-list body compile-env))
  )

(define (compile-mop3-nproc-args proc args env)
  (cond ((eq? proc quote)  args)				;DEBT better to use case
	((eq? proc set!)   (list (car args) (compile (cadr args) env)))
	((eq? proc lambda) (compile-lambda-args args env))
	((eq? proc if)     (compile-list args env))
	((eq? proc let)    (compile-let-args args env))
	((eq? proc cond)   (compile-condargs args env))
	((eq? proc begin)  (compile-list args env))
	((eq? proc when)   (compile-list args env))
	((eq? proc unless) (compile-list args env))
	((eq? proc while)  (compile-list args env))
	((eq? proc until)  (compile-list args env))
	((eq? proc and)    (compile-list args env))
	((eq? proc or)     (compile-list args env))
	;;add other mop3 nlambdas here, else leave the args to be fully interpreted
	(else args)
	)
  )

(define (compile-anyprocedure maker proc env)
  (let* ((me "(compile-anyprocedure)")
	 (lmda    (car proc))
	 (env_lm  (cdr proc))
	 (formals (car lmda))
	 (body    (cdr lmda))
	 )
    (maker formals (compile-list body env) env_lm)
    )
  )

(define (compile-procedure proc env)  (compile-anyprocedure make-procedure  proc env))
(define (compile-nprocedure proc env) (compile-anyprocedure make-nprocedure proc env))

(define (compile-symbol sym env)
  (let* ((def (lamb-variable-definition sym env)))
    (ifnot def sym				;if not defined then sym stays uncompiled
	   (let ((val (cdr def)))		;if defined as proc then return the proc, otherwise return uncompiled sym
	     (cond ((lamb-mop3-proc? val) val)
		   ((lamb-mop3-nproc? val) val)
		   ((procedure? val)  val)
		   ((nprocedure? val) val)
		   (else sym)
		   )
	     )
	   )
    )
  )

(define (compile sexpr env)
  (let ((me "(compile)"))
    (cond ((symbol? sexpr)     (compile-symbol sexpr env))
	  ((pair? sexpr)       (compile-pair sexpr env))
	  ((procedure? sexpr)  (compile-procedure sexpr env))
	  ((nprocedure? sexpr) (compile-nprocedure sexpr env))
	  (else sexpr)
	  )
    )
  )

(define testval 0)

(define testproc '(begin
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    (set! testval (+ 1 testval))
		    )
  )

(define testproc-compiled (compile testproc (current-environment)))

(set! testval 0)

(define t0 (micros))
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(eval testproc)
(define t1 (micros))
(define testval0 testval)

(set! testval 100)
(define t2 (micros))
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(eval testproc-compiled)
(define t3 (micros))
(define testval1 testval)

(news "t0 ~a v0 ~a t1 ~a v1 ~a\n" (- t1 t0) testval0 (- t3 t2)  testval1)
