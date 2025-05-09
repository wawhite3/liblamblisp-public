;;;
(syslog "Loading rxrs Scheme\n")

(define (make-promise done? proc) (list (cons done? proc)))

(define-syntax (delay-force expression) (make-promise #f (lambda () expression)))

(define-syntax (delay expression) (delay-force (make-promise #t expression)))

(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise)))
	    )
	(unless (promise-done? promise) (promise-update! promise* promise))
	(force promise)
	)
      )
  )

(define promise-done? (lambda (x) (caar x)))
(define promise-value (lambda (x) (cdar x)))

(define promise-update!
  (lambda (new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old))
    )
  )

(define-syntax map
  (lambda (proc . lists)
    (letrec ((map-all (lambda (proc lists result)
			(if (null? lists) (reverse! result)
			    (map-all proc (cdr lists) (cons (proc (car lists)) result))
			    )
			)
		      )
	     )
      (map-all proc lists nil)
      )
    )
  )
